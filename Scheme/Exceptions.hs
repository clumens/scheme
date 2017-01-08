{-# LANGUAGE OverloadedStrings #-}

module Scheme.Exceptions(defaultExceptionHandler,
                         errorEnvironment,
                         errorConstrFn,
                         errorPredFn,
                         divByZeroMessage,
                         internalErrorMessage,
                         ioErrorMessage,
                         numArgsMessage,
                         parseErrorMessage,
                         syntaxErrorMessage,
                         typeErrorMessage,
                         unboundErrorMessage,
                         undefinedErrorMessage)
 where

import Scheme.LispVal(IFunc(..), LispVal(..), unwordsList, showVal, typeOf)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           System.Exit(exitFailure)
import           System.IO(stderr)

errorEnvironment :: [(T.Text, LispVal)]
errorEnvironment = [
    -- Error types.
    ("base-error",              ErrorType Nothing),
    ("internal-error",          ErrorType Nothing),
    ("div-by-zero-error",       ErrorType $ Just "base-error"),
    ("io-error",                ErrorType $ Just "base-error"),
    ("syntax-error",            ErrorType $ Just "base-error"),
    ("parse-error",             ErrorType $ Just "syntax-error"),
    ("type-error",              ErrorType $ Just "base-error"),
    ("unbound-error",           ErrorType $ Just "base-error"),
    ("undefined-error",         ErrorType $ Just "base-error"),

    -- Error making functions.
    ("make-div-by-zero-error",  Func (IFunc $ \args -> return $ errorConstrFn "div-by-zero-error" args) Nothing),
    ("make-io-error",           Func (IFunc $ \args -> return $ errorConstrFn "io-error" args) Nothing),
    ("make-syntax-error",       Func (IFunc $ \args -> return $ errorConstrFn "syntax-error" args) Nothing),
    ("make-parse-error",        Func (IFunc $ \args -> return $ errorConstrFn "parse-error" args) Nothing),
    ("make-type-error",         Func (IFunc $ \args -> return $ errorConstrFn "type-error" args) Nothing),
    ("make-unbound-error",      Func (IFunc $ \args -> return $ errorConstrFn "unbound-error" args) Nothing),
    ("make-undefined-error",    Func (IFunc $ \args -> return $ errorConstrFn "undefined-error" args) Nothing)
 ]

-- This is the top-level exception handler.  It handles all exceptions that do not get
-- handled elsewhere.  This could either be an exception raised in scheme code that doesn't
-- get caught by a guard, or an exception of a type not handled in scheme, or an
-- exception in the Haskell side of things, or (the worst case) an exception raised by
-- Haskell itself.
--
-- For internal-errors, there's nothing to do but print the message and die.  We can no
-- longer trust the environment.
--
-- Other errors are less bad.  In the REPL case, these errors get printed out and the
-- user returned to the prompt.
defaultExceptionHandler :: LispVal -> IO ()
defaultExceptionHandler (Error "internal-error" msg) = do
    TIO.hPutStrLn stderr msg
    exitFailure
defaultExceptionHandler (Error ty msg) =
    TIO.hPutStrLn stderr $ T.concat ["Error: (", ty, "):\n", msg]
defaultExceptionHandler _ = return ()

errorConstrFn :: T.Text -> [LispVal] -> LispVal
errorConstrFn t [String msg]    = case t of
    "io-error"              -> Error t (ioErrorMessage msg)
    "parse-error"           -> Error t (parseErrorMessage msg)
    "syntax-error"          -> Error t (syntaxErrorMessage msg)
    "unbound-error"         -> Error t (unboundErrorMessage msg)
    "undefined-error"       -> Error t (undefinedErrorMessage msg)
    _                       -> Error t (T.concat ["\t", msg])
errorConstrFn _ [x]             = Error "type-error" (typeErrorMessage "String" x)
errorConstrFn _ x               = Error "syntax-error" (numArgsMessage 1 x)

errorPredFn :: [LispVal] -> LispVal
errorPredFn [Error _ _]         = Bool False
errorPredFn [x]                 = Error "type-error" (typeErrorMessage "Error" x)
errorPredFn x                   = Error "syntax-error" (numArgsMessage 1 x)

-- The following functions format an error message to be put into an exception
-- object.  Some of them don't do anything other than just return the provided
-- string, but they are here so formatting can be added later without having to
-- change everywhere an exception is generated.
--
-- In general, the type of the exception does not need to be a part of the
-- messages generated here because the exception object carries that information
-- with it and it will be printed out by the exception handler.

divByZeroMessage :: T.Text
divByZeroMessage = "\tDivide by zero"

internalErrorMessage :: T.Text -> T.Text
internalErrorMessage msg = T.concat ["*** INTERNAL ERROR: ", msg]

ioErrorMessage :: T.Text -> T.Text
ioErrorMessage msg = T.concat ["\t", msg]

numArgsMessage :: Int -> [LispVal] -> T.Text
numArgsMessage n args = T.concat ["\tExpected: ", T.pack $ show n, " arg(s)\n",
                                  "\tReceived: ", unwordsList args]

parseErrorMessage :: T.Text -> T.Text
parseErrorMessage msg = T.concat ["\t", msg]

syntaxErrorMessage :: T.Text -> T.Text
syntaxErrorMessage msg = T.concat ["\t", msg]

typeErrorMessage :: T.Text -> LispVal -> T.Text
typeErrorMessage expected got = T.concat ["\tExpected: ", expected, "\n",
                                          "\tGot:      ", typeOf got, "\n",
                                          "\tIn value: ", showVal got]

unboundErrorMessage :: T.Text -> T.Text
unboundErrorMessage atom = T.concat ["\tUnbound variable: ", atom]

undefinedErrorMessage :: T.Text -> T.Text
undefinedErrorMessage msg = T.concat ["\t", msg]
