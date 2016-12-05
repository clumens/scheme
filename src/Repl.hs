{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Repl(mainLoop)
 where

import Eval(evalText, runParseTest, safeExec)

import           Control.Monad.IO.Class(liftIO)
import qualified Data.Text as T
import           System.Console.Haskeline(InputT, defaultSettings, getInputLine, outputStrLn, runInputT)

type Repl a = InputT IO a

mainLoop :: IO ()
mainLoop = runInputT defaultSettings repl

repl :: Repl ()
repl = getInputLine "Repl> " >>= \case
    Nothing    -> outputStrLn "Goodbye."
    Just input -> liftIO (process input) >> repl
    --Just input -> (liftIO $ processToAST input) >> repl

process :: String -> IO ()
process str = do
  res <- safeExec $ evalText $ T.pack str
  either putStrLn return res

processToAST :: String -> IO ()
processToAST str = print $ runParseTest $ T.pack str
