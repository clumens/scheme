{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Scheme.Repl(mainLoop)
 where

import Scheme.Eval(execText, safeExec)
import Scheme.LispVal(EnvCtx)

import           Control.Monad.IO.Class(liftIO)
import           Data.Char(isSpace)
import qualified Data.Map as Map
import qualified Data.Text as T
import           System.Console.Haskeline

type Repl a = InputT IO a

searchFunc :: [T.Text] -> String -> [Completion]
searchFunc wds s = map (simpleCompletion . T.unpack) $ filter (T.pack s `T.isPrefixOf`) wds

replSettings :: [T.Text] -> Settings IO
replSettings wds = Settings { historyFile = Nothing,
                              autoAddHistory = True,
                              complete = completeWord Nothing " \t()" $ return . searchFunc wds }

mainLoop :: EnvCtx -> IO ()
mainLoop env = do
    let wds = Map.keys env
    -- Add special forms defined in Eval.hs.
    let wds' = wds ++ ["apply", "begin", "cond", "define", "else", "lambda", "let", "if", "quote"]
    runInputT (replSettings wds') (repl env)

repl :: EnvCtx -> Repl ()
repl env = getInputLine "repl> " >>= \case
    Nothing     -> outputStrLn "Goodbye."
    Just input  -> case dropWhile isSpace input of
                       "" -> repl env
                       i  -> liftIO (process env i) >>= repl
    -- Just input -> (liftIO $ processToAST input) >> repl env

process :: EnvCtx -> String -> IO EnvCtx
process env str = do
    res <- safeExec $ execText env $ T.pack str
    case res of
        Left err   -> putStrLn err >> return env
        Right env' -> return env'
