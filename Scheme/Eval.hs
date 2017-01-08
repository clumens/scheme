{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Scheme.Eval(basicEnv,
                   evalText,
                   evalFile,
                   runParseTest)
 where

import Scheme.Exceptions
import Scheme.LispVal(Eval(..), EnvCtx, IFunc(..), LispVal(..), showVal)
import Scheme.Parser(readExpr, readExprFile)
import Scheme.Prim(unop, primEnv)

import           Control.Monad(void)
import           Control.Monad.State(MonadState, get, modify, put, runStateT)
import           Data.List(nub)
import qualified Data.Text as T
import qualified Data.Map as Map
import           Data.Monoid((<>))

--
-- Working with environments.  This goes here instead of in its own file because the use of
-- readFn (and therefore textToEvalForm, and therefore...) would introduce an import loop.
-- Oh well.
--

-- The basic environment consists of the primitive environment (those functions that have to
-- be implemented in Haskell), plus this special "read" function, which is basically eval.
-- Do I even want that here?
basicEnv :: EnvCtx
basicEnv = Map.fromList $ primEnv
          <> [("read", Func (IFunc $ unop readFn) Nothing)]
          <> errorEnvironment

-- Temporarily augment the environment with a set of new bindings (which take precedence over
-- whatever was in the environment before), and execute fn in that environment.  Then restore
-- the environment.
augmentEnv :: MonadState EnvCtx m => [(T.Text, LispVal)] -> m b -> m b
augmentEnv newBindings fn = do
    oldEnv <- get
    modify (Map.union (Map.fromList newBindings))
    result <- fn
    put oldEnv
    return result

-- Temporarily replace the environment with a new one, and execute fn in that environment.  Then
-- restore the old environment.  This is useful for lambdas, which have their execution environment
-- packed up at definition time.
replaceEnv :: MonadState s m => s -> m b -> m b
replaceEnv newEnv fn = do
    oldEnv <- get
    put newEnv
    result <- fn
    put oldEnv
    return result

-- Look up a name in the environment and return its LispVal.
getVar :: LispVal ->  Eval LispVal
getVar (Atom atom) = do
    env <- get
    case Map.lookup atom env of
        Just x  -> return x
        Nothing -> return $ Error "unbound-error" (unboundErrorMessage atom)
getVar n = return $ Error "type-error" (typeErrorMessage "Atom" n)

--
-- Evaluation functions of various types
--

-- Evaluate a LispVal as given by the "read" function and run it through the pipeline as if it were
-- provided in the REPL.  This lets the user type in a string that would be scheme code and have it
-- executed in the same environment as everything else.
readFn :: LispVal -> Eval LispVal
readFn x = eval x >>= \case
    String txt -> textToEvalForm txt
    val        -> return $ Error "type-error" (typeErrorMessage "String" val)

-- Force the evaluation of some scheme by running the StateT monad.  Return any return value given by the
-- evaluation as well as the new environment.  This environment can in turn be fed back into the next
-- run of the REPL, allowing building up the environment with more bindings.
runASTinEnv :: EnvCtx -> Eval b -> IO (b, EnvCtx)
runASTinEnv code action = runStateT (unEval action) code

runParseTest :: T.Text -> T.Text
runParseTest input = either (T.pack . show) showVal $ readExpr input

-- Evaluate a single input expression against the given environment, returning the value and the new
-- environment.  We always return a value because it could be an Error.
evalText :: EnvCtx -> T.Text -> IO (LispVal, EnvCtx)
evalText env textExpr =
    runASTinEnv env $ textToEvalForm textExpr

-- Called by evalText - parse a single string of input, evaluate it, and display any resulting error message.
-- Having this function split out could be handy elsewhere (like in readFn, used by the "read" scheme function).
textToEvalForm :: T.Text -> Eval LispVal
textToEvalForm input = either (\err -> return $ Error "parse-error" (parseErrorMessage $ T.pack $ show err)) eval $ readExpr input

-- Evaluate several input expressions against the given environment, returning the value and the new
-- environment.  We always return a value because it could be an error.
evalFile :: EnvCtx -> T.Text -> IO (LispVal, EnvCtx)
evalFile env fileExpr =
    runASTinEnv env $ fileToEvalForm fileExpr

-- Called by evalFile - parse a string of input, evaluate it, and display any resulting error message.  Having
-- this function split out could be handy elsewhere, though that's not happening right now.
fileToEvalForm :: T.Text -> Eval LispVal
fileToEvalForm input = either (\err -> return $ Error "parse-error" (parseErrorMessage $ T.pack $ show err)) evalBody $ readExprFile input

--
-- Misc. helper functions
--

-- Do the execution of a lambda or named user-defined function.  The arguments have already been evaluated in
-- the function application portion of eval, so don't do that again here.  Bind the evaluated arguments to
-- the named parameters, add those bindings to the environment, and then execute the body of the function.
applyLambda :: LispVal -> [LispVal] -> [LispVal] -> Eval LispVal
applyLambda expr params args =
    augmentEnv (zipWith (\a b -> (extractVar a, b)) params args) $
        eval expr

-- Check that a LispVal is an Atom, raising an exception if this is not the case.  This is used in various places
-- to ensure that a name is given for a a variable, function, etc.
ensureAtom :: LispVal -> Eval LispVal
ensureAtom n@(Atom _) = return  n
ensureAtom n          = return $ Error "type-error" (typeErrorMessage "Atom" n)

-- Extract the name out of an Atom.
extractVar :: LispVal -> T.Text
extractVar (Atom atom) = atom

-- Given a list of (name value) pairs from a let-expression, extract just the names.
getNames :: [LispVal] -> [LispVal]
getNames (List [x@(Atom _), _]:xs) = x : getNames xs
getNames []                        = []
getNames _                         = [Error "syntax-error" (syntaxErrorMessage "let bindings list malformed")]

-- Given a list of (name value) pairs from a let-expression, extract just the values.
getVals :: [LispVal] -> [LispVal]
getVals (List [_, x]:xs) = x : getVals xs
getVals []               = []
getVals _                = [Error "syntax-error" (syntaxErrorMessage "let bindings list malformed")]

--
-- eval - This function is used to evaluate a single scheme expression.  It takes a lot of forms, because
-- there are a lot of possibilities.  This function can't handle a list of expressions, like would be found
-- in a file.  For that, see evalBody.
--

-- Primitive values - obvious.
eval :: LispVal -> Eval LispVal
eval n@(Atom _)     = getVar n
eval (Bool b)       = return $ Bool b
eval (Character c)  = return $ Character c
eval (List [])      = return Nil
eval Nil            = return Nil
eval (Number i)     = return $ Number i
eval (String s)     = return $ String s

-- Call a procedure, passing all the other values as arguments to the procedure.  This is
-- effectively a way of undoing a list, and passing the results to a procedure.  In the
-- argument list, the last item must be a list.
-- Example: (apply + 1 2 (list 3 4))
eval (List (Atom "apply":Atom proc:args)) = do
    funVar <- eval (Atom proc)
    xVal   <- mapM eval args

    case last xVal of
        List extra -> do let realArgs = init xVal ++ extra
                         case funVar of
                             Func (IFunc fn) Nothing      -> fn realArgs
                             Func (IFunc fn) (Just bound) -> replaceEnv bound (fn realArgs)
                             _                            -> return $ Error "type-error" (typeErrorMessage "Function" funVar)
        x       -> return $ Error "type-error" (typeErrorMessage "List" x)

-- Returns an unevaluated value.
-- Example: (quote 12)
eval (List [Atom "quote", val]) = return val

-- The standard if/then/else expression.  Both then and else clauses are required, unlike real scheme.
-- Example: (if #t 100 200)
eval (List [Atom "if", predicate, trueExpr, falseExpr]) = eval predicate >>= \case
    Bool True  -> eval trueExpr
    Bool False -> eval falseExpr
    x          -> return $ Error "type-error" (typeErrorMessage "Bool" x)
eval (List (Atom "if":_))  = return $ Error "syntax-error" (syntaxErrorMessage "(if <bool> <true-expr> <false-expr>)")

-- The cond expression, made up of a bunch of clauses.  The clauses are not in a list.  Each clause consists
-- of a test and an expression.  Tests are evaluated until one returns true, in which case the matching
-- expression is evaluated and returned as the result.  There can also be an else clause, which must be last.
eval (List (Atom "cond":clauses)) =
    tryOne clauses
 where
    -- Handle a single condition - evaluate the test and if it's true, return the evaluation of the
    -- expression.  If it's false, try the next condition.  If it's not boolean, raise an error.
    tryOne (List [test, expr]:rest) = eval test >>= \case
                                          Bool True  -> eval expr
                                          Bool False -> tryOne rest
                                          x          -> return $ Error "type-error" (typeErrorMessage "Bool" x)
    -- If there's just a test without an expression, return the evaluation of the test.
    tryOne [List [test]]            = eval test
    -- If we got here, we ran out of cases without seeing an else.  The return value is implementation
    -- defined behavior, so I'm just returning false.
    tryOne []                       = return $ Bool False
    tryOne (x:_)                    = return $ Error "type-error" (typeErrorMessage "Atom or Expr" x)

-- Evaluate a sequence of expressions.  The main value of this seems to be that it's like let, but doesn't
-- require any variable definitions.
-- Example: (begin (+ 1 2)
--                 (* 3 4)
--                 (- 10 5))
eval (List [Atom "begin", rest]) = evalBody rest
eval (List (Atom "begin":rest))  = evalBody $ List rest

-- Define a single variable, giving it the value found by evaluating expr.  The variable is added to the
-- global environment since define can only occur at the top-level or at the beginning of the body of a
-- begin or let expression.
-- Example: (define x "hello, world!")
--          (define y (lambda (x) (+ 1 x)))
eval (List [Atom "define", varAtom@(Atom _), expr]) = do
    evalVal <- eval expr
    modify (Map.insert (extractVar varAtom) evalVal)
    return varAtom

-- Define a function with a list of parameters (the first of which is the name of the function) and a body.
-- The list of parameters may optionally include a ".", in which case one parameter may follow the dot.  At
-- function application time, all arguments that do not match up with a formal parameter will be condensed
-- down into a list and that list will be passed as the single parameter after the dot.
-- Example: (define (inc x) (+ 1 x))
--          (define (list . objs) objs)
--          (define (map fn . lsts) ...)
eval (List [Atom "define", List (Atom name:params), expr]) =
    case break (== Atom ".") params of
        (p, [])                     -> evalNormalFunc name p
        (p, [Atom ".", Atom rest])  -> evalSpecialFunc name p rest
        _                           -> return $ Error "syntax-error" (syntaxErrorMessage "only one parameter may appear after '.'")
 where
    evalNormalFunc name' params' = do
        -- Done for side effect - make sure all params are atoms.
        mapM_ ensureAtom params'

        if nub params' /= params' then return $ Error "syntax-error" (syntaxErrorMessage "duplicate names given in formal parameters list")
        else do
            let fn = Func (IFunc $ applyLambda expr params') Nothing
            modify (Map.insert name' fn)
            return (Atom name')

    evalSpecialFunc name' params' extra = do
        -- Done for side effect - make sure all params are atoms.  extra was already checked via
        -- pattern matching.
        mapM_ ensureAtom params'

        -- Make sure parameter names aren't duplicated, and that includes the extra one.
        let allParams = params' ++ [Atom extra]

        if nub allParams /= allParams then return $ Error "syntax-error" (syntaxErrorMessage "duplicate names given in formal parameters list")
        else do
            let fn = Func (IFunc $ \args -> let (matched, rest) = splitAt (length params') args
                                            in  applyLambda expr (params' ++ [Atom extra])
                                                                 (matched ++ [List rest]))
                          Nothing
            modify (Map.insert name' fn)
            return (Atom name')

-- Define a new error condition type.  The condition must be a subclass of some other
-- condition type that already exists.  The other arguments are the name of a function that
-- is used to construct a new object of this type and the name of a function that is used
-- to determine whether an object is of this type or not.  The constructor function currently
-- only takes one parameter, an error message.
-- Example: (define-condition-type big-error base-error make-big-error big-error?)
eval (List [Atom "define-condition-type", Atom ty, Atom superTy, Atom constr, Atom predicate]) = do
    -- Verify superTy exists in the environment before doing anything else.

    -- The constructor is a function that takes a single argument (the error string) and returns
    -- a new Error value containing that argument.  We have to pack up the type of the error as
    -- well, so other functions can operate on it.  This will not be exposed to the user (except
    -- indirectly, through the predicate functions).
    let constrFn = Func (IFunc $ \args -> return $ errorConstrFn ty args) Nothing
    modify (Map.insert constr constrFn)

    -- The predicate is a function that takes a single argument (a condition object) and returns
    -- a boolean indicating whether that object is of this condition's type or any of its supertypes.
    let predFn = Func (IFunc $ \args -> return $ errorPredFn ty args) Nothing
    modify (Map.insert predicate predFn)

    -- Add the condition type to the environment.  Note that while conditions take an optional supertype,
    -- the optional part is only so a base condition can be defined in the primitive environment.  No
    -- user-defined condition can ever exist without a supertype.  We enforce that here with pattern
    -- matching on the define-condition-type call.
    modify (Map.insert ty (ErrorType $ Just superTy))
    return (Atom ty)

-- Evaluate an expression, handling exceptions if they occur.  Multiple types of exceptions
-- can be separately handled by using a clause for each.  An else clause is also supported.  If
-- no handler is found, the exception is re-raised.
-- Example: (guard (exn ((condition? exn) '12)) (make-io-error "oh no"))
--          (guard (exn (#f '12) (else '13)) (make-io-error "oh no"))
eval (List [Atom "guard", List (Atom var:clauses), body]) = eval body >>= \case
    -- If evaluating the body raised an error, put the error object into the
    -- environment with the name given by the guard and start evaluating clauses
    -- until one matches.
    err@(Error _ _) -> augmentEnv [(var, err)] $
                           tryOne err clauses
    -- No error was encountered, so just return the value of the body.
    val             -> return val
 where
    -- Handle a single condition - evaluate the test and if it's true, return the evaluation of the
    -- expression.  If it's false, try the next condition.  If it's not boolean, raise an error.  If
    -- another error was encountered while evaluating the test, raise that as a new error.
    tryOne err (List [test, expr]:rest) = eval test >>= \case
                                              e@(Error _ _) -> return e
                                              Bool True     -> eval expr
                                              Bool False    -> tryOne err rest
                                              x             -> return $ Error "type-error" (typeErrorMessage "Bool" x)
    -- If there's just a test without an expression, return the evaluation of the test.
    tryOne _   [List [test]]            = eval test
    -- If we got here, we ran out of cases without seeing an else.  Re-raise
    -- the original error.
    tryOne err []                       = return err
    tryOne _   (x:_)                    = return $ Error "type-error" (typeErrorMessage "Atom or Expr" x)
eval (List (Atom "guard":_)) = return $ Error "syntax-error" (syntaxErrorMessage "(guard (<var> <clause1> ... <clauseN>) <expr>)")

-- Locally define a list of variables, add them to the environment, and then execute the body in that
-- environment.
-- Example: (let ((x 1) (y 2)) (+ x y))
--          (let ((x 1)) (+ 1 x))
eval (List [Atom "let", List pairs, expr]) = do
    atoms <- mapM ensureAtom $ getNames pairs

    if nub atoms /= atoms then return $ Error "syntax-error" (syntaxErrorMessage "duplicate names given in let bindings")
    else do
        vals  <- mapM eval       $ getVals pairs
        augmentEnv (zipWith (\a b -> (extractVar a, b)) atoms vals) $
            evalBody expr

-- Named let allows you do give a name to the body of the let, and then refer to this name within the body.
-- This lets you define a recursive or looping function.  This function takes as parameters everything
-- defined in the bindings section of the let, and those bindings have the given initial values.
-- Example: (let loop ((x 10))
--                    (if (zero? x)
--                        (write "All done")
--                        (loop (dec x))))
eval (List [Atom "let", Atom name, List pairs, expr]) = do
    atoms <- mapM ensureAtom $ getNames pairs

    if nub atoms /= atoms then return $ Error "syntax-error" (syntaxErrorMessage "duplicate names given in let bindings")
    else do
        vals  <- mapM eval       $ getVals pairs

        -- Add another binding to the environment - a function with the name given, and whose body is the body
        -- of the let.  Also add the names of the variables defined in the let as the parameters to that function.
        let atoms'  = Atom name : atoms
        let fn      = Func (IFunc $ applyLambda expr atoms) Nothing
        let vals'   = fn : vals

        augmentEnv (zipWith (\a b -> (extractVar a, b)) atoms' vals') $
            evalBody expr
eval (List (Atom "let":_)) = return $ Error "syntax-error" (syntaxErrorMessage "(let <pair1> ... <pairN> <body>)")

-- Define a lambda function with a list of parameters (no name in this one) and a body.  We also grab the
-- current environment and pack that up with the lambda's definition.
-- Example: (lambda (x) (* 10 x))
eval (List [Atom "lambda", List params, expr]) =
    if nub params /= params then return $ Error "syntax-error" (syntaxErrorMessage "duplicate names given in lambda parameters")
    else do
        envLocal <- get
        return  $ Func (IFunc $ applyLambda expr params) (Just envLocal)
eval (List (Atom "lambda":_)) = return $ Error "syntax-error" (syntaxErrorMessage "(lambda (<params>) <body>)")

-- Function application, called when some word is encountered.  Check if that word is a function.  If
-- so, see if it's a primitive, lambda, or normal user-defined function.  Act appropriately.  For a
-- lambda, that means using the packed up environment instead of the current one.
-- Example: (inc 5)
eval (List (x:xs)) = do
    funVar <- eval x
    xVal   <- mapM eval xs
    case funVar of
        Func (IFunc fn) Nothing         -> fn xVal
        Func (IFunc fn) (Just bound)    -> replaceEnv bound (fn xVal)
        _                               -> return $ Error "type-error" (typeErrorMessage "Function" funVar)

-- If we made it all the way down here and couldn't figure out what sort of thing we're dealing with,
-- that's an error.
eval x = return $ Error "undefined-error" (undefinedErrorMessage $ T.concat ["Unknown error processing expression: ", showVal x])

--
-- evalBody - This function is used to evaluate an entire file, or the beginning of the body of a let
-- or begin expression.  In other words, it processes a list of things where definitions can occur
-- intead of just a single expression.  In general, the first part of the argument pattern matches a
-- single expression and "rest" holds everything else in the body.  "rest" gets processed by calling
-- evalBody on it again.
--

-- I have no idea when this form gets used.
evalBody :: LispVal -> Eval LispVal
evalBody (List [List (Atom "define":[Atom var, defExpr]), rest]) = do
    evalVal <- eval defExpr
    modify (Map.insert var evalVal)
    eval rest

-- Define a value, like so: (define x 1).
--                      or: (define x (lambda (y) 1))
evalBody (List (List (Atom "define":[Atom var, defExpr]):rest)) = do
    evalVal <- eval defExpr
    modify (Map.insert var evalVal)
    evalBody $ List rest

-- Define a function, like so: (define (add x y) (+ x y))
--                         or: (define (id x) x)
evalBody (List (defn@(List [Atom "define", List _, _]):rest)) = do
    -- Evaluate the definition of the function using the eval version above.  We ignore the
    -- return value because there's no need to do anything with it.  That function handles adding
    -- it to the environment.
    void $ eval defn
    evalBody $ List rest

-- Catch anything that didn't get matched already.
evalBody x = eval x
