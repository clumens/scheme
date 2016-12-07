{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Repl(mainLoop)
 where

import Eval(evalText, runParseTest, safeExec)
import LispVal(EnvCtx)

import           Control.Monad.IO.Class(liftIO)
import qualified Data.Text as T
import           System.Console.Haskeline(InputT, defaultSettings, getInputLine, outputStrLn, runInputT)

type Repl a = InputT IO a

mainLoop :: EnvCtx -> IO ()
mainLoop env = runInputT defaultSettings (repl env)

repl :: EnvCtx -> Repl ()
repl env = getInputLine "Repl> " >>= \case
    Nothing    -> outputStrLn "Goodbye."
    Just input -> liftIO (process env input) >>= repl
    -- Just input -> (liftIO $ processToAST input) >> repl env

process :: EnvCtx -> String -> IO EnvCtx
process env str = do
    res <- safeExec $ evalText env $ T.pack str
    case res of
        Left err   -> putStrLn err >> return env
        Right env' -> return env'

processToAST :: String -> IO ()
processToAST str = print $ runParseTest $ T.pack str
