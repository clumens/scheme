{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Scheme.Eval(basicEnv,
                   evalText,
                   evalFile,
                   runParseTest,
                   safeExec)
 where

import Scheme.Exceptions(LispException(..))
import Scheme.LispVal(Eval(..), EnvCtx, IFunc(..), LispVal(..), showVal)
import Scheme.Parser(readExpr, readExprFile)
import Scheme.Prim(unop, primEnv)

import           Control.Exception(SomeException, fromException, throw, try)
import           Control.Monad(void)
import           Control.Monad.State(get, modify, put, runStateT)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
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
          <> [("read", PrimitiveFunc $ IFunc $ unop readFn)]

-- Temporarily augment the environment with a set of new bindings (which take precedence over
-- whatever was in the environment before), and execute fn in that environment.  Then restore
-- the environment.
augmentEnv newBindings fn = do
    oldEnv <- get
    modify (Map.union (Map.fromList newBindings))
    result <- fn
    put oldEnv
    return result

-- Temporarily replace the environment with a new one, and execute fn in that environment.  Then
-- restore the old environment.  This is useful for lambdas, which have their execution environment
-- packed up at definition time.
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
        Nothing -> throw $ UnboundVar atom
getVar n = throw $ TypeMismatch  "failure to get variable: " n

--
-- Evaluation functions of various types
--

-- Evaluate a LispVal as given by the "read" function and run it through the pipeline as if it were
-- provided in the REPL.  This lets the user type in a string that would be scheme code and have it
-- executed in the same environment as everything else.
readFn :: LispVal -> Eval LispVal
readFn x = eval x >>= \case
    String txt -> textToEvalForm txt
    val        -> throw $ TypeMismatch "read expects string, instead got: " val

-- Catch any exceptions raised by evaluation and convert them into an Either value.  This can then be
-- printed to the screen in the REPL (or just to the console if being run non-interactively, I guess).
safeExec :: IO a -> IO (Either String a)
safeExec m = try m >>= \case
    Left (eTop :: SomeException) -> case fromException eTop of
        Just (enclosed :: LispException) -> return $ Left (show enclosed)
        Nothing                          -> return $ Left (show eTop)
    Right val -> return $ Right val

-- Force the evaluation of some scheme by running the StateT monad.  Return any return value given by the
-- evaluation as well as the new environment.  This environment can in turn be fed back into the next
-- run of the REPL, allowing building up the environment with more bindings.
runASTinEnv :: EnvCtx -> Eval b -> IO (b, EnvCtx)
runASTinEnv code action = runStateT (unEval action) code

runParseTest :: T.Text -> T.Text
runParseTest input = either (T.pack . show) showVal $ readExpr input

-- The next two functions are for evaluating a string of input, which had better be just a single scheme
-- expression.  This is used by the REPL.

-- Evaluate a single input expression against the given environment, returning the new environment.
-- The environment could have been augmented with new bindings.
evalText :: EnvCtx -> T.Text -> IO EnvCtx
evalText env textExpr = do
    (result, env') <- runASTinEnv env $ textToEvalForm textExpr
    TIO.putStrLn $ showVal result
    return env'

-- Called by evalText - parse a single string of input, evaluate it, and display any resulting error message.
-- Having this function split out could be handy elsewhere (like in readFn, used by the "read" scheme function.
textToEvalForm :: T.Text -> Eval LispVal
textToEvalForm input = either (throw . PError . show) eval $ readExpr input

-- The next two functions are for evaluating a string of input, which could be many scheme expressions, as
-- would happen when reading a file from disk.  This is useful for reading in a standard library, or some user
-- provided file.

-- Evaluate several input expressions against the given environment, returning the new environment.
-- The environment could have been augmented with new bindings.
evalFile :: EnvCtx -> T.Text -> IO EnvCtx
evalFile env fileExpr = do
    (_, env') <- runASTinEnv env $ fileToEvalForm fileExpr
    return env'

-- Called by evalFile - parse a string of input, evaluate it, and display any resulting error message.  Having
-- this function split out could be handy elsewhere, though that's not happening right now.
fileToEvalForm :: T.Text -> Eval LispVal
fileToEvalForm input = either (throw . PError . show) evalBody $ readExprFile input

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
ensureAtom n = throw $ TypeMismatch "expected an atomic value" n

-- Extract the name out of an Atom.
extractVar :: LispVal -> T.Text
extractVar (Atom atom) = atom

-- Given a list of (name value) pairs from a let-expression, extract just the names.
getNames :: [LispVal] -> [LispVal]
getNames (List [x@(Atom _), _]:xs) = x : getNames xs
getNames []                        = []
getNames _                         = throw $ BadSpecialForm "let bindings list malformed"

-- Given a list of (name value) pairs from a let-expression, extract just the values.
getVals :: [LispVal] -> [LispVal]
getVals (List [_, x]:xs) = x : getVals xs
getVals []               = []
getVals _                = throw $ BadSpecialForm "let bindings list malformed"

--
-- eval - This function is used to evaluate a single scheme expression.  It takes a lot of forms, because
-- there are a lot of possibilities.  This function can't handle a list of expressions, like would be found
-- in a file.  For that, see evalBody.  Some complicated expression forms end up calling evalBody on their
-- own.
--

-- Primitive values - obvious.
eval :: LispVal -> Eval LispVal
eval (Number i) = return $ Number i
eval (String s) = return $ String s
eval (Bool b)   = return $ Bool b
eval (List [])  = return Nil
eval Nil        = return Nil
eval n@(Atom _) = getVar n

-- Returns an unevaluated value.
-- Example: (quote 12)
eval (List [Atom "quote", val]) = return val

-- The standard if/then/else expression.  Both then and else clauses are required, unlike real scheme.
-- Example: (if #t 100 200)
eval (List [Atom "if", predicate, trueExpr, falseExpr]) = eval predicate >>= \case
    Bool True  -> eval trueExpr
    Bool False -> eval falseExpr
    _          -> throw $ BadSpecialForm "if's first arg must eval into a boolean"
eval (List (Atom "if":_))  = throw $ BadSpecialForm "(if <bool> <s-expr> <s-expr>)"

-- The cond expression, made up of a bunch of clauses.  The clauses are not in a list.  Each clause consists
-- of a test and an expression.  Tests are evaluated until one returns true, in which case the matching
-- expression is evaluated and returned as the result.  There can also be an else clause, which must be last.
eval (List (Atom "cond":clauses)) =
    tryOne clauses
 where
    -- Handle the else case - return whatever expression is matched up with it.
    tryOne [List [Atom "else", expr]]   = eval expr
    -- Handle a single condition - evaluate the test and if it's true, return the evaluation of the
    -- expression.  If it's false, try the next condition.  If it's not boolean, raise an error.
    tryOne (List [test, expr]:rest)     = eval test >>= \case
                                              Bool True  -> eval expr
                                              Bool False -> tryOne rest
                                              x          -> throw $ TypeMismatch "Expected bool in cond test, got: " x
    -- If there's just a test without an expression, return the evaluation of the test.
    tryOne [List [test]]                = eval test
    -- If we got here, we ran out of cases without seeing an else.  The return value is implementation
    -- defined behavior, so I'm just returning false.
    tryOne []                           = return $ Bool False

-- Evaluate a sequence of expressions.  The main value of this seems to be that it's like let, but doesn't
-- require any variable definitions.
-- Example: (begin (+ 1 2)
--                 (* 3 4)
--                 (- 10 5))
eval (List [Atom "begin", rest]) = evalBody rest
eval (List (Atom "begin":rest))  = evalBody $ List rest

-- Define a single variable, giving it the value found by evaluating expr.  The variable is added to the
-- global environment since define can only occur at the top-level.
-- Example: (define x "hello, world!")
--          (define y (lambda (x) (+ 1 x)))
-- FIXME:  Is that restriction being enforced?
eval (List [Atom "define", varAtom@(Atom _), expr]) = do
    evalVal <- eval expr
    modify (Map.insert (extractVar varAtom) evalVal)
    return varAtom

-- Define a function with a list of parameters (the first of which is the name of the function) and a body.
-- The function is added to the global environment since define can only occur at the top-level.
-- Example: (define (inc x) (+ 1 x))
-- FIXME:  Is that restriction being enforced?
eval (List [Atom "define", List params, expr]) = do
    varParams <- mapM ensureAtom params
    let name = head varParams
    let fn = Func (map extractVar $ tail varParams)
                  (IFunc $ applyLambda expr params)
    modify (Map.insert (extractVar name) fn)
    return name

-- Locally define a list of variables, add them to the environment, and then execute the body in that
-- environment.
-- Example: (let ((x 1) (y 2)) (+ x y))
--          (let ((x 1)) (+ 1 x))
eval (List [Atom "let", List pairs, expr]) = do
    atoms <- mapM ensureAtom $ getNames pairs
    vals  <- mapM eval       $ getVals pairs
    augmentEnv (zipWith (\a b -> (extractVar a, b)) atoms vals) $
        evalBody expr
eval (List (Atom "let":_) ) = throw $ BadSpecialForm "lambda function expects list of parameters and S-Expression body\n(let <pairs> <s-expr>)"

-- Define a lambda function with a list of parameters (no name in this one) and a body.  We also grab the
-- current environment and pack that up with the lambda's definition.
-- Example: (lambda (x) (* 10 x))
eval (List [Atom "lambda", List params, expr]) = do
    envLocal <- get
    return  $ Lambda (IFunc $ applyLambda expr params) envLocal
eval (List (Atom "lambda":_) ) = throw $ BadSpecialForm "lambda function expects list of parameters and S-Expression body\n(lambda <params> <s-expr>)"

-- Function application, called when some word is encountered.  Check if that word is a function.  If
-- so, see if it's a primitive, lambda, or normal user-defined function.  Act appropriately.  For a
-- lambda, that means using the packed up environment instead of the current one.
-- Example: (inc 5)
eval (List (x:xs)) = do
    funVar <- eval x
    xVal   <- mapM eval xs
    case funVar of
        PrimitiveFunc (IFunc internalFn)    -> internalFn xVal
        Func params (IFunc internalFn)      -> augmentEnv (zip params xVal) (internalFn [])
        Lambda (IFunc internalFn) boundenv  -> replaceEnv boundenv (internalFn xVal)
        _                                   -> throw $ NotFunction funVar

-- If we made it all the way down here and couldn't figure out what sort of thing we're dealing with,
-- that's an error.
eval x = throw $ Default  x

--
-- evalBody - This function is used to evaluate an entire file, the body of a let expression, and the
-- body of a begin expression.  In other words, it processes a list of things instead of just a single
-- expression.  In general, the first part of the argument pattern matches a single expression and
-- "rest" holds everything else in the body.  The rest gets processed by calling evalBody on it again.
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
