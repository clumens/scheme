{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Scheme.Exceptions(LispException(..),
                         errorEnvironment,
                         errorConstrFn,
                         errorPredFn,
                         numArgsMessage,
                         showError)
 where

import Scheme.LispVal(IFunc(..), LispVal(..), unwordsList, showVal, typeOf)

import           Control.Exception(Exception, throw)
import qualified Data.Text as T
import           Data.Typeable(Typeable)

data LispException = LengthOfList T.Text Int
                   | TypeMismatch T.Text LispVal
                   | BadSpecialForm T.Text
                   | NotFunction LispVal
                   | UnboundVar T.Text
                   | Default LispVal
                   | PError String
                   | IOError T.Text
                   | Unknown T.Text
 deriving (Typeable)

instance Exception LispException

instance Show LispException where
  show = T.unpack . showError

showError :: LispException -> T.Text
showError err = case err of
    IOError txt            -> T.concat ["Error reading file: ", txt]
    LengthOfList txt int   -> T.concat ["Error Length of List in ", txt, " length: ", T.pack $ show int]
    TypeMismatch got val   -> T.concat ["Error Type Mismatch, got: ", got, " expected: ", typeOf val, " in value: ", showVal val]
    BadSpecialForm txt     -> T.concat ["Error Bad Special Form: ", txt]
    NotFunction val        -> T.concat ["Error Not a Function: ", showVal val]
    UnboundVar txt         -> T.concat ["Error Unbound Variable: ", txt]
    Unknown txt            -> T.concat ["Error: ", txt]
    PError str             -> T.concat ["Parser Error, expression cannot evaluate: ",T.pack str]
    Default val            -> T.concat ["Error, Danger Will Robinson! Evaluation could not proceed!  ", showVal val]

errorEnvironment :: [(T.Text, LispVal)]
errorEnvironment = [
    -- Error types.
    ("base-error",      ErrorType Nothing),
    ("io-error",        ErrorType $ Just "base-error"),             -- IOError
    ("syntax-error",    ErrorType $ Just "base-error"),             -- BadSpecialForm, LengthOfList
    ("parse-error",     ErrorType $ Just "syntax-error"),           -- PError
    ("type-error",      ErrorType $ Just "base-error"),             -- NotFunction, TypeMismatch
    ("unbound-error",   ErrorType $ Just "base-error"),             -- UnboundVar
    ("undefined-error", ErrorType $ Just "base-error"),             -- Default, Unknown

    -- Error making functions.
    ("make-base-error",         Func (IFunc $ \args -> return $ errorConstrFn "base-error" args) Nothing),
    ("make-io-error",           Func (IFunc $ \args -> return $ errorConstrFn "io-error" args) Nothing),
    ("make-syntax-error",       Func (IFunc $ \args -> return $ errorConstrFn "syntax-error" args) Nothing),
    ("make-parse-error",        Func (IFunc $ \args -> return $ errorConstrFn "parse-error" args) Nothing),
    ("make-type-error",         Func (IFunc $ \args -> return $ errorConstrFn "type-error" args) Nothing),
    ("make-unbound-error",      Func (IFunc $ \args -> return $ errorConstrFn "unbound-error" args) Nothing),
    ("make-undefined-error",    Func (IFunc $ \args -> return $ errorConstrFn "undefined-error" args) Nothing)
 ]

errorConstrFn :: T.Text -> [LispVal] -> LispVal
errorConstrFn t [String msg]    = Error t msg
errorConstrFn _ [x]             = throw $ TypeMismatch "condition constructor expects String" x
errorConstrFn _ x               = Error "syntax-error" (numArgsMessage 1 x)

errorPredFn :: [LispVal] -> LispVal
errorPredFn [Error _ _]         = Bool False
errorPredFn [x]                 = throw $ TypeMismatch "predicate function expected Error" x
errorPredFn x                   = Error "syntax-error" (numArgsMessage 1 x)

numArgsMessage :: Int -> [LispVal] -> T.Text
numArgsMessage n args = T.concat ["\tExpected: ", T.pack $ show n, " arg(s)\n",
                                  "\tReceived: ", unwordsList args]
