{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Eval(basicEnv,
            evalText,
            evalFile,
            runParseTest,
            safeExec)
 where

import Exceptions(LispException(..))
import LispVal(Eval(..), EnvCtx, IFunc(..), LispVal(..))
import Parser(readExpr, readExprFile)
import Prim(unop, primEnv)

import           Control.Exception(SomeException, fromException, throw, try)
import           Control.Monad.Trans.Resource
import           Control.Monad.State(get, modify, put, runStateT)
import qualified Data.Text as T
import qualified Data.Map as Map
import           Data.Monoid((<>))

basicEnv :: EnvCtx
basicEnv = Map.fromList $ primEnv
          <> [("read", PrimitiveFunc $ IFunc $ unop readFn)]

augmentEnv newBindings fn = do
    oldEnv <- get
    modify (Map.union (Map.fromList newBindings))
    result <- fn
    put oldEnv
    return result

replaceEnv newEnv fn = do
    oldEnv <- get
    put newEnv
    result <- fn
    put oldEnv
    return result

readFn :: LispVal -> Eval LispVal
readFn x = eval x >>= \case
    String txt -> textToEvalForm txt
    val        -> throw $ TypeMismatch "read expects string, instead got: " val

safeExec :: IO a -> IO (Either String a)
safeExec m = try m >>= \case
    Left (eTop :: SomeException) -> case fromException eTop of
        Just (enclosed :: LispException) -> return $ Left (show enclosed)
        Nothing                          -> return $ Left (show eTop)
    Right val -> return $ Right val

runASTinEnv :: EnvCtx -> Eval b -> IO (b, EnvCtx)
runASTinEnv code action = runStateT (unEval action) code

evalText :: EnvCtx -> T.Text -> IO EnvCtx
evalText env textExpr = do
    (result, env') <- runASTinEnv env $ textToEvalForm textExpr
    print result
    return env'

textToEvalForm :: T.Text -> Eval LispVal
textToEvalForm input = either (throw . PError . show ) eval $ readExpr input

evalFile :: EnvCtx -> T.Text -> IO EnvCtx
evalFile env fileExpr = do
    (result, env') <- runASTinEnv env $ fileToEvalForm fileExpr
    print result
    return env'

fileToEvalForm :: T.Text -> Eval LispVal
fileToEvalForm input = either (throw . PError . show ) evalBody $ readExprFile input

runParseTest :: T.Text -> T.Text
runParseTest input = either (T.pack . show) (T.pack . show) $ readExpr input

getVar :: LispVal ->  Eval LispVal
getVar (Atom atom) = do
    env <- get
    case Map.lookup atom env of
        Just x  -> return x
        Nothing -> throw $ UnboundVar atom
getVar n = throw $ TypeMismatch  "failure to get variable: " n

ensureAtom :: LispVal -> Eval LispVal
ensureAtom n@(Atom _) = return  n
ensureAtom n = throw $ TypeMismatch "expected an atomic value" n

extractVar :: LispVal -> T.Text
extractVar (Atom atom) = atom

getNames (List [x@(Atom _), _]:xs) = x : getNames xs
getNames []                        = []
getNames _                         = throw $ BadSpecialForm "let bindings list malformed"

getVals (List [_, x]:xs) = x : getVals xs
getVals []               = []
getVals _                = throw $ BadSpecialForm "let bindings list malformed"

applyLambda :: LispVal -> [LispVal] -> [LispVal] -> Eval LispVal
applyLambda expr params args = do
    argEval <- mapM eval args
    augmentEnv (zipWith (\a b -> (extractVar a, b)) params argEval) $
        eval expr

eval :: LispVal -> Eval LispVal
eval (Number i) = return $ Number i
eval (String s) = return $ String s
eval (Bool b)   = return $ Bool b
eval (List [])  = return Nil
eval Nil        = return Nil
eval n@(Atom _) = getVar n

eval (List [Atom "write", rest])      = return . String . T.pack $ show rest
eval (List ((:) (Atom "write") rest)) = return . String . T.pack . show $ List rest

eval (List [Atom "quote", val]) = return val

eval (List [Atom "if", pred, truExpr, flsExpr]) = eval pred >>= \case
    Bool True  -> eval truExpr
    Bool False -> eval flsExpr
    _          -> throw $ BadSpecialForm "if's first arg must eval into a boolean"
eval args@(List ( (:) (Atom "if") _)) = throw $ BadSpecialForm "(if <bool> <s-expr> <s-expr>)"

eval (List [Atom "begin", rest]) = evalBody rest
eval (List ((:) (Atom "begin") rest )) = evalBody $ List rest

eval (List [Atom "define", varAtom@(Atom _), expr]) = do
    evalVal <- eval expr
    modify (Map.insert (extractVar varAtom) evalVal)
    return varAtom

eval (List [Atom "define", List params, expr]) = do
    varParams <- mapM ensureAtom params
    let name = head varParams
    let fn = Func (map extractVar $ tail varParams)
                  (IFunc $ applyLambda expr params)
    modify (Map.insert (extractVar name) fn)
    return name

eval (List [Atom "let", List pairs, expr]) = do
    atoms <- mapM ensureAtom $ getNames pairs
    vals  <- mapM eval       $ getVals pairs
    augmentEnv (zipWith (\a b -> (extractVar a, b)) atoms vals) $
        evalBody expr
eval (List (Atom "let":_) ) = throw $ BadSpecialForm "lambda function expects list of parameters and S-Expression body\n(let <pairs> <s-expr>)"

eval (List [Atom "lambda", List params, expr]) = do
    envLocal <- get
    return  $ Lambda (IFunc $ applyLambda expr params) envLocal
eval (List (Atom "lambda":_) ) = throw $ BadSpecialForm "lambda function expects list of parameters and S-Expression body\n(lambda <params> <s-expr>)"

eval (List ((:) x xs)) = do
    funVar <- eval x
    xVal   <- mapM eval xs
    case funVar of
        PrimitiveFunc (IFunc internalFn)    -> internalFn xVal
        Func params (IFunc internalFn)      -> augmentEnv (zip params xVal) (internalFn [])
        Lambda (IFunc internalFn) boundenv  -> replaceEnv boundenv (internalFn xVal)
        _                                   -> throw $ NotFunction funVar

eval x = throw $ Default  x

evalBody :: LispVal -> Eval LispVal
evalBody (List [List ((:) (Atom "define") [Atom var, defExpr]), rest]) = do
    evalVal <- eval defExpr
    modify (Map.insert var evalVal)
    eval rest

evalBody (List ((:) (List ((:) (Atom "define") [Atom var, defExpr])) rest)) = do
    evalVal <- eval defExpr
    modify (Map.insert var evalVal)
    evalBody $ List rest
evalBody x = eval x
