{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Scheme.Exceptions(catchHaskellExceptions,
                         defaultExceptionHandler,
                         errorEnvironment,
                         errorTypeEnvironment,
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

import Scheme.LispVal(IFunc(..), LispVal(..), SchemeSt(..), mkEmptyState, unwordsList, showVal, typeOf)
import Scheme.Types(SchemeTy(..))

import           Control.Exception(SomeException, try)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           System.Exit(exitFailure)
import           System.IO(stderr)

errorTypeEnvironment :: [(T.Text, SchemeTy)]
errorTypeEnvironment = [
    -- Error types.
    ("base-error",              Condition Nothing),
    ("internal-error",          Condition Nothing),
    ("div-by-zero-error",       Condition $ Just "base-error"),
    ("io-error",                Condition $ Just "base-error"),
    ("syntax-error",            Condition $ Just "base-error"),
    ("parse-error",             Condition $ Just "syntax-error"),
    ("type-error",              Condition $ Just "base-error"),
    ("unbound-error",           Condition $ Just "base-error"),
    ("undefined-error",         Condition $ Just "base-error")
 ]

errorEnvironment :: [(T.Text, LispVal)]
errorEnvironment = [
    -- Error making functions.
    ("make-div-by-zero-error",  Func (IFunc $ \args -> return $ errorConstrFn "div-by-zero-error" args) Nothing),
    ("make-io-error",           Func (IFunc $ \args -> return $ errorConstrFn "io-error" args) Nothing),
    ("make-syntax-error",       Func (IFunc $ \args -> return $ errorConstrFn "syntax-error" args) Nothing),
    ("make-parse-error",        Func (IFunc $ \args -> return $ errorConstrFn "parse-error" args) Nothing),
    ("make-type-error",         Func (IFunc $ \args -> return $ errorConstrFn "type-error" args) Nothing),
    ("make-unbound-error",      Func (IFunc $ \args -> return $ errorConstrFn "unbound-error" args) Nothing),
    ("make-undefined-error",    Func (IFunc $ \args -> return $ errorConstrFn "undefined-error" args) Nothing),

    -- Error predication functions.
    ("div-by-zero-error?",      Func (IFunc $ \args -> return $ errorPredFn "div-by-zero-error" args) Nothing),
    ("io-error?",               Func (IFunc $ \args -> return $ errorPredFn "io-error" args) Nothing),
    ("syntax-error?",           Func (IFunc $ \args -> return $ errorPredFn "syntax-error" args) Nothing),
    ("parse-error?",            Func (IFunc $ \args -> return $ errorPredFn "parse-error" args) Nothing),
    ("type-error?",             Func (IFunc $ \args -> return $ errorPredFn "type-error" args) Nothing),
    ("unbound-error?",          Func (IFunc $ \args -> return $ errorPredFn "unbound-error" args) Nothing),
    ("undefined-error?",        Func (IFunc $ \args -> return $ errorPredFn "undefined-error" args) Nothing)
 ]

-- Catch any Haskell exceptions raised by evaluation (which shouldn't happen, but that's why they're
-- called exceptions) and convert them into a LispVal Error.  This can then be handled like any
-- exception that occurred in scheme.
catchHaskellExceptions :: IO (LispVal, SchemeSt) -> IO (LispVal, SchemeSt)
catchHaskellExceptions m = try m >>= \case
    -- It seems odd that we're returning an empty environment here.  However, internal errors are
    -- bad and we can't recover from them.  The default exception handler will cause the
    -- interpreter to shut down so it doesn't really matter what environment we return.
    Left (exn :: SomeException) -> return (Error "internal-error" (internalErrorMessage $ T.pack $ show exn), mkEmptyState)
    Right val                   -> return val

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
-- div-by-zero-error takes no arguments.
errorConstrFn "div-by-zero-error" []    = Error "div-by-zero-error" divByZeroMessage
errorConstrFn "div-by-zero-error" x     = Error "syntax-error" (numArgsMessage 0 x)
-- io-error takes one argument: A message
errorConstrFn "io-error" [String msg]   = Error "io-error" (ioErrorMessage msg)
errorConstrFn "io-error" [x]            = Error "type-error" (typeErrorMessage "String" x)
errorConstrFn "io-error" x              = Error "syntax-error" (numArgsMessage 1 x)
-- syntax-error takes one argument: A message
-- FIXME: Maybe it should take two, the other being the expression
errorConstrFn "syntax-error" [String msg]   = Error "syntax-error" (syntaxErrorMessage msg)
errorConstrFn "syntax-error" [x]            = Error "type-error" (typeErrorMessage "String" x)
errorConstrFn "syntax-error" x              = Error "syntax-error" (numArgsMessage 1 x)
-- parse-error takes one argument: A message
errorConstrFn "parse-error" [String msg]    = Error "parse-error" (parseErrorMessage msg)
errorConstrFn "parse-error" [x]             = Error "type-error" (typeErrorMessage "String" x)
errorConstrFn "parse-error" x               = Error "syntax-error" (numArgsMessage 1 x)
-- type-error takes two arguments: The expected type, and the actual value that had an error.
errorConstrFn "type-error" [String expected, x] = Error "type-error" (typeErrorMessage expected x)
errorConstrFn "type-error" [x, _]               = Error "type-error" (typeErrorMessage "String" x)
errorConstrFn "type-error" x                    = Error "syntax-error" (numArgsMessage 2 x)
-- unbound-error takes one argument: A message
errorConstrFn "unbound-error" [String msg]  = Error "unbound-error" (unboundErrorMessage msg)
errorConstrFn "unbound-error" [x]           = Error "type-error" (typeErrorMessage "String" x)
errorConstrFn "unbound-error" x             = Error "syntax-error" (numArgsMessage 2 x)
-- undefined-error takes one argument: A message
errorConstrFn "undefined-error" [String msg]    = Error "undefined-error" (undefinedErrorMessage msg)
errorConstrFn "undefined-error" [x]             = Error "type-error" (typeErrorMessage "String" x)
errorConstrFn "undefined-error" x               = Error "syntax-error" (numArgsMessage 2 x)
-- You can't create a base-error or internal-error via scheme, so getting here with
-- one of those is an error.
errorConstrFn "base-error"      _   = Error "syntax-error" (syntaxErrorMessage "Directly creating a base-error is not allowed")
errorConstrFn "internal-error"  _   = Error "internal-error" (syntaxErrorMessage "Directly creating an internal-error is not allowed")
-- Any other value is a user-created error.  Those can only take one argument (for now).
errorConstrFn t [String msg]    = Error t (T.concat ["\t", msg])
errorConstrFn _ [x]             = Error "type-error" (typeErrorMessage "String" x)
errorConstrFn _ x               = Error "syntax-error" (numArgsMessage 1 x)

errorPredFn :: T.Text -> [LispVal] -> LispVal
errorPredFn t [Error ty _]      = Bool $ t == ty
errorPredFn _ [x]               = Error "type-error" (typeErrorMessage "Error" x)
errorPredFn _ x                 = Error "syntax-error" (numArgsMessage 1 x)

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
