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
                         listErrorMessage,
                         numArgsMessage,
                         parseErrorMessage,
                         syntaxErrorMessage,
                         typeErrorMessage,
                         unboundErrorMessage,
                         undefinedErrorMessage)
 where

import Scheme.Values(IFunc(..), SchemeSt(..), Value(..), mkEmptyState, unwordsList, showVal, typeOf)
import Scheme.Types(SchemeTy(..))

import           Control.Exception(SomeException, try)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           System.Exit(exitFailure)
import           System.IO(stderr)

errorTypeEnvironment :: [(T.Text, SchemeTy)]
errorTypeEnvironment = [
    -- Error types.
    ("base-error",              CondTy Nothing),
    ("internal-error",          CondTy Nothing),
    ("div-by-zero-error",       CondTy $ Just "base-error"),
    ("io-error",                CondTy $ Just "base-error"),
    ("list-error",              CondTy $ Just "list-error"),
    ("syntax-error",            CondTy $ Just "base-error"),
    ("parse-error",             CondTy $ Just "syntax-error"),
    ("type-error",              CondTy $ Just "base-error"),
    ("unbound-error",           CondTy $ Just "base-error"),
    ("undefined-error",         CondTy $ Just "base-error")
 ]

errorEnvironment :: [(T.Text, Value)]
errorEnvironment = [
    -- Error making functions.
    ("make-div-by-zero-error",  Func (IFunc $ \args -> return $ errorConstrFn "div-by-zero-error" args) Nothing),
    ("make-io-error",           Func (IFunc $ \args -> return $ errorConstrFn "io-error" args) Nothing),
    ("make-list-error",         Func (IFunc $ \args -> return $ errorConstrFn "list-error" args) Nothing),
    ("make-syntax-error",       Func (IFunc $ \args -> return $ errorConstrFn "syntax-error" args) Nothing),
    ("make-parse-error",        Func (IFunc $ \args -> return $ errorConstrFn "parse-error" args) Nothing),
    ("make-type-error",         Func (IFunc $ \args -> return $ errorConstrFn "type-error" args) Nothing),
    ("make-unbound-error",      Func (IFunc $ \args -> return $ errorConstrFn "unbound-error" args) Nothing),
    ("make-undefined-error",    Func (IFunc $ \args -> return $ errorConstrFn "undefined-error" args) Nothing),

    -- Error predication functions.
    ("div-by-zero-error?",      Func (IFunc $ \args -> return $ errorPredFn "div-by-zero-error" args) Nothing),
    ("io-error?",               Func (IFunc $ \args -> return $ errorPredFn "io-error" args) Nothing),
    ("list-error?",             Func (IFunc $ \args -> return $ errorPredFn "list-error" args) Nothing),
    ("syntax-error?",           Func (IFunc $ \args -> return $ errorPredFn "syntax-error" args) Nothing),
    ("parse-error?",            Func (IFunc $ \args -> return $ errorPredFn "parse-error" args) Nothing),
    ("type-error?",             Func (IFunc $ \args -> return $ errorPredFn "type-error" args) Nothing),
    ("unbound-error?",          Func (IFunc $ \args -> return $ errorPredFn "unbound-error" args) Nothing),
    ("undefined-error?",        Func (IFunc $ \args -> return $ errorPredFn "undefined-error" args) Nothing)
 ]

-- Catch any Haskell exceptions raised by evaluation (which shouldn't happen, but that's why they're
-- called exceptions) and convert them into a Value Error.  This can then be handled like any
-- exception that occurred in scheme.
catchHaskellExceptions :: IO (Value, SchemeSt) -> IO (Value, SchemeSt)
catchHaskellExceptions m = try m >>= \case
    -- It seems odd that we're returning an empty environment here.  However, internal errors are
    -- bad and we can't recover from them.  The default exception handler will cause the
    -- interpreter to shut down so it doesn't really matter what environment we return.
    Left (exn :: SomeException) -> return (Raised "internal-error" (internalErrorMessage $ T.pack $ show exn), mkEmptyState)
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
defaultExceptionHandler :: Value -> IO ()
defaultExceptionHandler (Raised "internal-error" msg) = do
    TIO.hPutStrLn stderr msg
    exitFailure
defaultExceptionHandler (Raised ty msg) =
    TIO.hPutStrLn stderr $ T.concat ["Error: (", ty, "):\n", msg]
defaultExceptionHandler _ = return ()

errorConstrFn :: T.Text -> [Value] -> Value
-- div-by-zero-error takes no arguments.
errorConstrFn "div-by-zero-error" []    = Condition "div-by-zero-error" divByZeroMessage
errorConstrFn "div-by-zero-error" x     = Condition "syntax-error" (numArgsMessage 0 x)
-- io-error takes one argument: A message
errorConstrFn "io-error" [String msg]   = Condition "io-error" (ioErrorMessage msg)
errorConstrFn "io-error" [x]            = Condition "type-error" (typeErrorMessage "String" x)
errorConstrFn "io-error" x              = Condition "syntax-error" (numArgsMessage 1 x)
-- list-error takes one argument: The function where an empty list occurred
errorConstrFn "list-error" [String fn]  = Condition "list-error" (listErrorMessage fn)
errorConstrFn "list-error" [x]          = Condition "type-error" (typeErrorMessage "String" x)
errorConstrFn "list-error" x            = Condition "syntax-error" (numArgsMessage 1 x)
-- syntax-error takes one argument: A message
-- FIXME: Maybe it should take two, the other being the expression
errorConstrFn "syntax-error" [String msg]   = Condition "syntax-error" (syntaxErrorMessage msg)
errorConstrFn "syntax-error" [x]            = Condition "type-error" (typeErrorMessage "String" x)
errorConstrFn "syntax-error" x              = Condition "syntax-error" (numArgsMessage 1 x)
-- parse-error takes one argument: A message
errorConstrFn "parse-error" [String msg]    = Condition "parse-error" (parseErrorMessage msg)
errorConstrFn "parse-error" [x]             = Condition "type-error" (typeErrorMessage "String" x)
errorConstrFn "parse-error" x               = Condition "syntax-error" (numArgsMessage 1 x)
-- type-error takes two arguments: The expected type, and the actual value that had an error.
errorConstrFn "type-error" [String expected, x] = Condition "type-error" (typeErrorMessage expected x)
errorConstrFn "type-error" [x, _]               = Condition "type-error" (typeErrorMessage "String" x)
errorConstrFn "type-error" x                    = Condition "syntax-error" (numArgsMessage 2 x)
-- unbound-error takes one argument: A message
errorConstrFn "unbound-error" [String msg]  = Condition "unbound-error" (unboundErrorMessage msg)
errorConstrFn "unbound-error" [x]           = Condition "type-error" (typeErrorMessage "String" x)
errorConstrFn "unbound-error" x             = Condition "syntax-error" (numArgsMessage 2 x)
-- undefined-error takes one argument: A message
errorConstrFn "undefined-error" [String msg]    = Condition "undefined-error" (undefinedErrorMessage msg)
errorConstrFn "undefined-error" [x]             = Condition "type-error" (typeErrorMessage "String" x)
errorConstrFn "undefined-error" x               = Condition "syntax-error" (numArgsMessage 2 x)
-- You can't create a base-error or internal-error via scheme, so getting here with
-- one of those is an error.
errorConstrFn "base-error"      _   = Condition "syntax-error" (syntaxErrorMessage "Directly creating a base-error is not allowed")
errorConstrFn "internal-error"  _   = Condition "internal-error" (syntaxErrorMessage "Directly creating an internal-error is not allowed")
-- Any other value is a user-created error.  Those can only take one argument (for now).
errorConstrFn t [String msg]    = Condition t (T.concat ["\t", msg])
errorConstrFn _ [x]             = Condition "type-error" (typeErrorMessage "String" x)
errorConstrFn _ x               = Condition "syntax-error" (numArgsMessage 1 x)

errorPredFn :: T.Text -> [Value] -> Value
errorPredFn t [Condition ty _]  = Bool $ t == ty
errorPredFn _ [x]               = Raised "type-error" (typeErrorMessage "Error" x)
errorPredFn _ x                 = Raised "syntax-error" (numArgsMessage 1 x)

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

listErrorMessage :: T.Text -> T.Text
listErrorMessage fn = T.concat ["\tEmpty list encountered in function: ", fn]

numArgsMessage :: Int -> [Value] -> T.Text
numArgsMessage n args = T.concat ["\tExpected: ", T.pack $ show n, " arg(s)\n",
                                  "\tReceived: ", unwordsList args]

parseErrorMessage :: T.Text -> T.Text
parseErrorMessage msg = T.concat ["\t", msg]

syntaxErrorMessage :: T.Text -> T.Text
syntaxErrorMessage msg = T.concat ["\t", msg]

typeErrorMessage :: T.Text -> Value -> T.Text
typeErrorMessage expected got = T.concat ["\tExpected: ", expected, "\n",
                                          "\tGot:      ", typeOf got, "\n",
                                          "\tIn value: ", showVal got]

unboundErrorMessage :: T.Text -> T.Text
unboundErrorMessage atom = T.concat ["\tUnbound variable: ", atom]

undefinedErrorMessage :: T.Text -> T.Text
undefinedErrorMessage msg = T.concat ["\t", msg]
