{-# LANGUAGE OverloadedStrings #-}

module Scheme.Exceptions(errorEnvironment,
                         errorConstrFn,
                         errorPredFn,
                         numArgsMessage,
                         typeErrorMessage)
 where

import Scheme.LispVal(IFunc(..), LispVal(..), unwordsList, showVal, typeOf)

import qualified Data.Text as T

errorEnvironment :: [(T.Text, LispVal)]
errorEnvironment = [
    -- Error types.
    ("base-error",      ErrorType Nothing),
    ("io-error",        ErrorType $ Just "base-error"),
    ("syntax-error",    ErrorType $ Just "base-error"),
    ("parse-error",     ErrorType $ Just "syntax-error"),
    ("type-error",      ErrorType $ Just "base-error"),
    ("unbound-error",   ErrorType $ Just "base-error"),
    ("undefined-error", ErrorType $ Just "base-error"),

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
errorConstrFn _ [x]             = Error "type-error" (typeErrorMessage "String" x)
errorConstrFn _ x               = Error "syntax-error" (numArgsMessage 1 x)

errorPredFn :: [LispVal] -> LispVal
errorPredFn [Error _ _]         = Bool False
errorPredFn [x]                 = Error "type-error" (typeErrorMessage "Error" x)
errorPredFn x                   = Error "syntax-error" (numArgsMessage 1 x)

numArgsMessage :: Int -> [LispVal] -> T.Text
numArgsMessage n args = T.concat ["\tExpected: ", T.pack $ show n, " arg(s)\n",
                                  "\tReceived: ", unwordsList args]

typeErrorMessage :: T.Text -> LispVal -> T.Text
typeErrorMessage expected got = T.concat ["\tExpected: ", expected, "\n",
                                          "\tGot:      ", typeOf got, "\n",
                                          "\tIn value: ", showVal got]
