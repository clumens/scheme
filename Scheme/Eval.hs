{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Scheme.Eval(initialState,
                   evalText,
                   evalFile,
                   runParseTest)
 where

import Scheme.Environment
import Scheme.Exceptions
import Scheme.Values(Eval(..), IFunc(..), SchemeSt(..), Value(..), mkEmptyState, showVal)
import Scheme.Parser(readExpr, readExprFile)
import Scheme.Prim(unop, primEnv)
import Scheme.Types(SchemeTy(..))

import           Control.Monad.State(MonadState, get, modify, put, runStateT)
import           Data.List(nub)
import qualified Data.Text as T
import           Data.Monoid((<>))

--
-- Working with environments.  This goes here instead of in its own file because the use of
-- readFn (and therefore textToEvalForm, and therefore...) would introduce an import loop.
-- Oh well.
--

-- The initial program state consists of two parts:
-- (1) The primitive bindings environment (those functions that have to be implemented in
-- Haskell), plus the special "read" function, which is basically eval, defined here.
-- (2) The type environment, which for now just holds error types.
--
-- FIXME:  Without "read", this could live in Value.hs instead.  That would be a nicer
-- place for it.
initialState :: SchemeSt
initialState = mkEmptyState { stBindings=mkEnvironment $ primEnv
                                         <> [("read", Func (IFunc $ unop readFn) Nothing)]
                                         <> errorEnvironment,
                              stTypes=mkEnvironment errorTypeEnvironment }

-- Temporarily augment the bindings environment with a set of new bindings (which take precedence
-- over whatever was in the environment before), and execute fn in that environment.  Then restore
-- the environment.
withAugmentedEnv :: MonadState SchemeSt m => [(T.Text, Value)] -> m b -> m b
withAugmentedEnv newBindings fn = do
    oldState <- get
    modify (\st -> st { stBindings=addToEnvironment newBindings (stBindings st) })
    result <- fn
    put oldState
    return result

-- Temporarily replace the bindings environment with a new one, and execute fn in that environment.
-- Then, restore the old environment.  This is useful for lambdas, which have their execution
-- environment packed up at definition time.
withReplacedEnv :: MonadState s m => s -> m b -> m b
withReplacedEnv newEnv fn = do
    oldState <- get
    put newEnv
    result <- fn
    put oldState
    return result

-- Look up a name in the environment and return its Value.
getVar :: Value ->  Eval Value
getVar (Atom atom) = do
    env <- stBindings <$> get
    case environmentLookup atom env of
        Just x  -> return x
        Nothing -> return $ Raised "unbound-error" (unboundErrorMessage atom)
getVar n = return $ Raised "type-error" (typeErrorMessage "Atom" n)

--
-- Evaluation functions of various types
--

-- Evaluate a Value as given by the "read" function and run it through the pipeline as if it were
-- provided in the REPL.  This lets the user type in a string that would be scheme code and have it
-- executed in the same environment as everything else.
readFn :: Value -> Eval Value
readFn x = eval x >>= \case
    String txt -> textToEvalForm txt
    val        -> return $ Raised "type-error" (typeErrorMessage "String" val)

-- Force the evaluation of some scheme by running the StateT monad.  Return any return value given by the
-- evaluation as well as the new state.  This state can in turn be fed back into the next run of the
-- REPL, allowing building up more bindings.
runASTinEnv :: SchemeSt -> Eval b -> IO (b, SchemeSt)
runASTinEnv state action = runStateT (unEval action) state

runParseTest :: T.Text -> T.Text
runParseTest input = either (T.pack . show) showVal $ readExpr input

-- Evaluate a single input expression against the given state, returning the value and the new state.
-- We always return a value because it could be an Error.
evalText :: SchemeSt -> T.Text -> IO (Value, SchemeSt)
evalText state textExpr =
    runASTinEnv state $ textToEvalForm textExpr

-- Called by evalText - parse a single string of input, evaluate it, and display any resulting error message.
-- Having this function split out could be handy elsewhere (like in readFn, used by the "read" scheme function).
textToEvalForm :: T.Text -> Eval Value
textToEvalForm input = either (\err -> return $ Raised "parse-error" (parseErrorMessage $ T.pack $ show err)) eval $ readExpr input

-- Evaluate several input expressions against the given state , returning the value and the new state
-- We always return a value because it could be an error.
evalFile :: SchemeSt -> T.Text -> IO (Value, SchemeSt)
evalFile state fileExpr =
    runASTinEnv state $ fileToEvalForm fileExpr

-- Called by evalFile - parse a string of input, evaluate it, and display any resulting error message.  Having
-- this function split out could be handy elsewhere, though that's not happening right now.
fileToEvalForm :: T.Text -> Eval Value
fileToEvalForm input = either (\err -> return $ Raised "parse-error" (parseErrorMessage $ T.pack $ show err)) evalBody $ readExprFile input

--
-- Misc. helper functions
--

-- Do the execution of a lambda or named user-defined function.  The arguments have already been evaluated in
-- the function application portion of eval, so don't do that again here.  Bind the evaluated arguments to
-- the named parameters, add those bindings to the environment, and then execute the body of the function.
applyLambda :: Value -> [Value] -> [Value] -> Eval Value
applyLambda expr params args =
    withAugmentedEnv (zipWith (\a b -> (extractVar a, b)) params args) $
        eval expr

-- Check that a Value is an Atom, raising an exception if this is not the case.  This is used in various places
-- to ensure that a name is given for a a variable, function, etc.
ensureAtom :: Value -> Eval Value
ensureAtom n@(Atom _) = return n
ensureAtom n          = return $ Raised "type-error" (typeErrorMessage "Atom" n)

-- Extract the name out of an Atom.
extractVar :: Value -> T.Text
extractVar (Atom atom) = atom

-- Given a list of (name value) pairs from a let-expression, extract just the names.
getNames :: [Value] -> [Value]
getNames (List [x@(Atom _), _]:xs) = x : getNames xs
getNames []                        = []
getNames _                         = [Raised "syntax-error" (syntaxErrorMessage "let bindings list malformed")]

-- Given a list of (name value) pairs from a let-expression, extract just the values.
getVals :: [Value] -> [Value]
getVals (List [_, x]:xs) = x : getVals xs
getVals []               = []
getVals _                = [Raised "syntax-error" (syntaxErrorMessage "let bindings list malformed")]

-- Evaluate a list of expressions (like, the parameters to a function).  If one results in an
-- error, stop evaluating and return just that error.  If there are no errors, return the results
-- as a list.
mapEval :: [Value] -> Eval (Either Value [Value])
mapEval lst = doit [] lst
 where
    doit accum []       = return $ Right accum
    doit accum (x:xs)   = eval x >>= \case
        err@(Raised _ _) -> return $ Left err
        e                -> doit (accum ++ [e]) xs

-- Make an error message for the extremely unlikely case that mapEval ever returns something
-- other than a [Value] or an Error.  But it makes -Wall happy.
mkMapEvalError :: Value -> Eval Value
mkMapEvalError x = return $ Raised "internal-error" (internalErrorMessage $ T.concat ["mapEval returned unexpected value: ", showVal x])

--
-- eval - This function is used to evaluate a single scheme expression.  It takes a lot of forms, because
-- there are a lot of possibilities.  This function can't handle a list of expressions, like would be found
-- in a file.  For that, see evalBody.
--

-- Primitive values - obvious.
eval :: Value -> Eval Value
eval n@(Atom _)         = getVar n
eval (Bool b)           = return $ Bool b
eval (Character c)      = return $ Character c
eval (Float f)          = return $ Float f
eval e@(Condition _ _)  = return e
eval (List [])          = return Nil
eval Nil                = return Nil
eval (Number i)         = return $ Number i
eval e@(Raised _ _)     = return e
eval (String s)         = return $ String s

-- Call a procedure, passing all the other values as arguments to the procedure.  This is
-- effectively a way of undoing a list, and passing the results to a procedure.  In the
-- argument list, the last item must be a list.
-- Example: (apply + 1 2 (list 3 4))
eval (List (Atom "apply":Atom proc:args)) =
    mapEval args >>= \case
        Left (err@(Raised _ _))             -> return err
        Left x                              -> mkMapEvalError x
        Right (reverse -> (List extra):xs)  -> do let realArgs = xs ++ extra
                                                  eval (Atom proc) >>= \case
                                                      Func (IFunc fn) Nothing       -> fn realArgs
                                                      Func (IFunc fn) (Just bound)  -> withReplacedEnv bound (fn realArgs)
                                                      e                             -> return $ Raised "type-error" (typeErrorMessage "Function" e)
        Right (reverse -> x:_)              -> return $ Raised "type-error" (typeErrorMessage "List" x)
        Right _                             -> return $ Raised "syntax-error" (syntaxErrorMessage "(apply <proc> <arg1> ... <argN>)")
eval (List (Atom "apply":_)) = return $ Raised "syntax-error" (syntaxErrorMessage "(apply <proc> <arg1> ... <argN>)")

-- Returns an unevaluated value.
-- Example: (quote 12)
eval (List [Atom "quote", val]) = return val

-- The standard if/then/else expression.  Both then and else clauses are required, unlike real scheme.
-- Example: (if #t 100 200)
eval (List [Atom "if", predicate, trueExpr, falseExpr]) = eval predicate >>= \case
    err@(Raised _ _) -> return err
    Bool True        -> eval trueExpr
    Bool False       -> eval falseExpr
    x                -> return $ Raised "type-error" (typeErrorMessage "Bool" x)
eval (List (Atom "if":_))  = return $ Raised "syntax-error" (syntaxErrorMessage "(if <bool> <true-expr> <false-expr>)")

-- The cond expression, made up of a bunch of clauses.  The clauses are not in a list.  Each clause consists
-- of a test and an expression.  Tests are evaluated until one returns true, in which case the matching
-- expression is evaluated and returned as the result.  There can also be an else clause, which must be last.
eval (List (Atom "cond":clauses)) =
    tryOne clauses
 where
    -- Handle a single condition - evaluate the test and if it's true, return the evaluation of the
    -- expression.  If it's false, try the next condition.  If it's not boolean, raise an error.
    tryOne (List [test, expr]:rest) = eval test >>= \case
                                          e@(Raised _ _) -> return e
                                          Bool True      -> eval expr
                                          Bool False     -> tryOne rest
                                          x              -> return $ Raised "type-error" (typeErrorMessage "Bool" x)
    -- If there's just a test without an expression, return the evaluation of the test.
    tryOne [List [test]]            = eval test
    -- If we got here, we ran out of cases without seeing an else.  The return value is implementation
    -- defined behavior, so I'm just returning false.
    tryOne []                       = return $ Bool False
    tryOne (x:_)                    = return $ Raised "type-error" (typeErrorMessage "Atom or Expr" x)

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
eval (List [Atom "define", varAtom@(Atom _), expr]) =
    eval expr >>= \case
        err@(Raised _ _) -> return err
        evalVal          -> do modify (\st -> st { stBindings=addToEnvironment [(extractVar varAtom, evalVal)] (stBindings st) })
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
        _                           -> return $ Raised "syntax-error" (syntaxErrorMessage "only one parameter may appear after '.'")
 where
    evalNormalFunc name' params' = do
        -- Done for side effect - make sure all params are atoms.
        mapM_ ensureAtom params'

        if nub params' /= params' then return $ Raised "syntax-error" (syntaxErrorMessage "duplicate names given in formal parameters list")
        else do
            let fn = Func (IFunc $ applyLambda expr params') Nothing
            modify (\st -> st { stBindings=addToEnvironment [(name', fn)] (stBindings st) })
            return (Atom name')

    evalSpecialFunc name' params' extra = do
        -- Done for side effect - make sure all params are atoms.  extra was already checked via
        -- pattern matching.
        mapM_ ensureAtom params'

        -- Make sure parameter names aren't duplicated, and that includes the extra one.
        let allParams = params' ++ [Atom extra]

        if nub allParams /= allParams then return $ Raised "syntax-error" (syntaxErrorMessage "duplicate names given in formal parameters list")
        else do
            let fn = Func (IFunc $ \args -> let (matched, rest) = splitAt (length params') args
                                            in  applyLambda expr (params' ++ [Atom extra])
                                                                 (matched ++ [List rest]))
                          Nothing
            modify (\st -> st { stBindings=addToEnvironment [(name', fn)] (stBindings st) })
            return (Atom name')

-- Define a new error condition type.  The condition must be a subclass of some other
-- condition type that already exists.  The other arguments are the name of a function that
-- is used to construct a new object of this type and the name of a function that is used
-- to determine whether an object is of this type or not.  The constructor function currently
-- only takes one parameter, an error message.
-- Example: (define-condition-type big-error base-error make-big-error big-error?)
eval (List [Atom "define-condition-type", Atom ty, Atom superTy, Atom constr, Atom predicate]) = do
    -- Verify superTy exists in the environment before doing anything else.
    env <- stTypes <$> get
    case environmentLookup superTy env of
        Just (CondTy _) -> do
            -- The constructor is a function that takes a single argument (the error string) and returns
            -- a new Error value containing that argument.  We have to pack up the type of the error as
            -- well, so other functions can operate on it.  This will not be exposed to the user (except
            -- indirectly, through the predicate functions).
            let constrFn = Func (IFunc $ \args -> return $ errorConstrFn ty args) Nothing
            modify (\st -> st { stBindings=addToEnvironment [(constr, constrFn)] (stBindings st) })

            -- The predicate is a function that takes a single argument (a condition object) and returns
            -- a boolean indicating whether that object is of this condition's type or any of its supertypes.
            let predFn = Func (IFunc $ \args -> return $ errorPredFn ty args) Nothing
            modify (\st -> st { stBindings=addToEnvironment [(predicate, predFn)] (stBindings st) })

            -- Add the condition type to the environment.  Note that while conditions take an optional supertype,
            -- the optional part is only so a base condition can be defined in the primitive environment.  No
            -- user-defined condition can ever exist without a supertype.  We enforce that here with pattern
            -- matching on the define-condition-type call.
            modify (\st -> st { stTypes=addToEnvironment [(ty, CondTy $ Just superTy)] (stTypes st) })
            return (Atom ty)

        _ -> return $ Raised "syntax-error" (syntaxErrorMessage $ T.concat [superTy, " is not a valid condition type"])
eval (List (Atom "define-condition-type":_)) = return $ Raised "syntax-error" (syntaxErrorMessage "(define-condition-type ty super constr pred)")

-- Evaluate an expression, handling exceptions if they occur.  Multiple types of exceptions
-- can be separately handled by using a clause for each.  An else clause is also supported.  If
-- no handler is found, the exception is re-raised.
-- Example: (guard (exn ((condition? exn) '12)) (make-io-error "oh no"))
--          (guard (exn (#f '12) (else '13)) (make-io-error "oh no"))
eval (List [Atom "guard", List (Atom var:clauses), body]) = eval body >>= \case
    -- If evaluating the body raised an error, put the error object into the
    -- environment with the name given by the guard and start evaluating clauses
    -- until one matches.
    Raised ty msg -> withAugmentedEnv [(var, Condition ty msg)] $
                         tryOne (Condition ty msg) clauses
    -- No error was encountered, so just return the value of the body.
    val             -> return val
 where
    -- Handle a single condition - evaluate the test and if it's true, return the evaluation of the
    -- expression.  If it's false, try the next condition.  If it's not boolean, raise an error.  If
    -- another error was encountered while evaluating the test, raise that as a new error.
    tryOne err (List [test, expr]:rest) = eval test >>= \case
                                              e@(Raised _ _) -> return e
                                              Bool True      -> eval expr
                                              Bool False     -> tryOne err rest
                                              x              -> return $ Raised "type-error" (typeErrorMessage "Bool" x)
    -- If there's just a test without an expression, return the evaluation of the test.
    tryOne _   [List [test]]            = eval test
    -- If we got here, we ran out of cases without seeing an else.  Re-raise
    -- the original error.
    tryOne err []                       = return err
    tryOne _   (x:_)                    = return $ Raised "type-error" (typeErrorMessage "Atom or Expr" x)
eval (List (Atom "guard":_)) = return $ Raised "syntax-error" (syntaxErrorMessage "(guard (<var> <clause1> ... <clauseN>) <expr>)")

-- Locally define a list of variables, add them to the environment, and then execute the body in that
-- environment.
-- Example: (let ((x 1) (y 2)) (+ x y))
--          (let ((x 1)) (+ 1 x))
eval (List [Atom "let", List pairs, expr]) = do
    atoms <- mapM ensureAtom $ getNames pairs

    if nub atoms /= atoms
    then return $ Raised "syntax-error" (syntaxErrorMessage "duplicate names given in let bindings")
    else mapEval (getVals pairs) >>= \case
        Left (err@(Raised _ _)) -> return err
        Left x                  -> mkMapEvalError x
        Right vals              -> withAugmentedEnv (zipWith (\a b -> (extractVar a, b)) atoms vals) $
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

    if nub atoms /= atoms
    then return $ Raised "syntax-error" (syntaxErrorMessage "duplicate names given in let bindings")
    else mapEval (getVals pairs) >>= \case
        Left (err@(Raised _ _)) -> return err
        Left x                  -> mkMapEvalError x
        Right vals              -> do
            -- Add another binding to the environment - a function with the name given, and whose body is the body
            -- of the let.  Also add the names of the variables defined in the let as the parameters to that function.
            let atoms'  = Atom name : atoms
            let fn      = Func (IFunc $ applyLambda expr atoms) Nothing
            let vals'   = fn : vals

            withAugmentedEnv (zipWith (\a b -> (extractVar a, b)) atoms' vals') $
                evalBody expr
eval (List (Atom "let":_)) = return $ Raised "syntax-error" (syntaxErrorMessage "(let <pair1> ... <pairN> <body>)")

-- Define a lambda function with a list of parameters (no name in this one) and a body.  We also grab the
-- current environment and pack that up with the lambda's definition.  The list of parameters may
-- optionally include a ".", in which case one parameter may follow the dot.  At function application time,
-- all arguments that do not match up with a format parameter will be condensed down into a list and that
-- list will be passed as the single parameter after the dot.
-- Example: (lambda (x) (* 10 x))
--          ((lambda x x) 3 4 5 6)
--          ((lambda (x y . z) z) 3 4 5 6)
eval (List [Atom "lambda", List params, expr]) =
    case break (== Atom ".") params of
        (p, [])                     -> evalNormalFunc p
        (p, [Atom ".", Atom rest])  -> evalSpecialFunc p rest
        _                           -> return $ Raised "syntax-error" (syntaxErrorMessage "only one parameter may appear after '.'")
 where
    evalNormalFunc params' = do
        -- Done for side effect - make sure all params are atoms.
        mapM_ ensureAtom params'

        if nub params' /= params' then return $ Raised "syntax-error" (syntaxErrorMessage "duplicate names given in lambda parameters")
        else do
            envLocal <- get
            return $ Func (IFunc $ applyLambda expr params) (Just envLocal)

    evalSpecialFunc params' extra = do
        -- Done for side effect - make sure all params are atoms.  extra was already checked via
        -- pattern matching.
        mapM_ ensureAtom params'

        -- Make sure parameter names aren't duplicated, and that includes the extra one.
        let allParams = params' ++ [Atom extra]

        if nub allParams /= allParams then return $ Raised "syntax-error" (syntaxErrorMessage "duplicate names given in lambda parameters")
        else do
            envLocal <- get
            return $ Func (IFunc $ \args -> let (matched, rest) = splitAt (length params') args
                                            in  applyLambda expr (params' ++ [Atom extra])
                                                                 (matched ++ [List rest]))
                          (Just envLocal)
eval (List [Atom "lambda", Atom param, expr]) = do
    -- In this case, the lambda takes a single parameter and all arguments get converted into a list
    -- and bound to that parameter.  "define" does the same thing, but the syntax is different for
    -- lambda because lambdas do not have a name.  It's too bad this can't be shared with
    -- evalSpecialFunc up above, like how we treat "define".
    envLocal <- get
    return $ Func (IFunc $ \args -> applyLambda expr [Atom param] [List args]) (Just envLocal)
eval (List (Atom "lambda":_)) = return $ Raised "syntax-error" (syntaxErrorMessage "(lambda (<params>) <body>)")

-- Function application, called when some word is encountered.  Check if that word is a function.  If
-- so, see if it's a primitive, lambda, or normal user-defined function.  Act appropriately.  For a
-- lambda, that means using the packed up environment instead of the current one.
-- Example: (inc 5)
eval (List (x:xs)) = eval x >>= \case
    err@(Raised _ _)                -> return err
    Func (IFunc fn) Nothing         -> do mapEval xs >>= \case
                                              Left (err@(Raised _ _))   -> return err
                                              Left e                    -> mkMapEvalError e
                                              Right xVal                -> fn xVal
    Func (IFunc fn) (Just bound)    -> do mapEval xs >>= \case
                                              Left (err@(Raised _ _))   -> return err
                                              Left e                    -> mkMapEvalError e
                                              Right xVal                -> withReplacedEnv bound (fn xVal)
    _                               -> return $ Raised "type-error" (typeErrorMessage "Function" x)

-- If we made it all the way down here and couldn't figure out what sort of thing we're dealing with,
-- that's an error.
eval x = return $ Raised "undefined-error" (undefinedErrorMessage $ T.concat ["Unknown error processing expression: ", showVal x])

--
-- evalBody - This function is used to evaluate an entire file, or the beginning of the body of a let
-- or begin expression.  In other words, it processes a list of things where definitions can occur
-- intead of just a single expression.  In general, the first part of the argument pattern matches a
-- single expression and "rest" holds everything else in the body.  "rest" gets processed by calling
-- evalBody on it again.
--

-- I have no idea when this form gets used.
evalBody :: Value -> Eval Value
evalBody (List [List (Atom "define":[Atom var, defExpr]), rest]) =
    eval defExpr >>= \case
        err@(Raised _ _) -> return err
        evalVal          -> do modify (\st -> st { stBindings=addToEnvironment [(var, evalVal)] (stBindings st) })
                               eval rest

-- Define a value, like so: (define x 1).
--                      or: (define x (lambda (y) 1))
evalBody (List (List (Atom "define":[Atom var, defExpr]):rest)) =
    eval defExpr >>= \case
        err@(Raised _ _) -> return err
        evalVal          -> do modify (\st -> st { stBindings=addToEnvironment [(var, evalVal)] (stBindings st) })
                               evalBody $ List rest

-- Define a function, like so: (define (add x y) (+ x y))
--                         or: (define (id x) x)
evalBody (List (defn@(List [Atom "define", List _, _]):rest)) =
    -- Evaluate the definition of the function using the eval version above.  We ignore the
    -- return value because there's no need to do anything with it.  That function handles adding
    -- it to the environment.
    eval defn >>= \case
        err@(Raised _ _) -> return err
        _                -> evalBody $ List rest

-- Catch anything that didn't get matched already.
evalBody x = eval x
