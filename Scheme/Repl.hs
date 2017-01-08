{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Scheme.Repl(mainLoop)
 where

import Scheme.Eval(execText, safeExec)
import Scheme.LispVal(EnvCtx)

import           Control.Monad.IO.Class(liftIO)
import           Data.Char(isSpace)
import           Data.List(sort)
import qualified Data.Map as Map
import qualified Data.Text as T
import           System.Console.Haskeline
import           System.Exit(exitSuccess)

type Repl a = InputT IO a

searchFunc :: [T.Text] -> String -> [Completion]
searchFunc wds s = map (simpleCompletion . T.unpack) $ filter (T.pack s `T.isPrefixOf`) wds

replSettings :: [T.Text] -> Settings IO
replSettings wds = Settings { historyFile = Nothing,
                              autoAddHistory = True,
                              complete = completeWord Nothing " \t()" $ return . searchFunc wds }

validWords :: EnvCtx -> [T.Text]
validWords env = sort $
    Map.keys env ++
    -- Add special words defined in Eval.hs.
    ["apply", "begin", "cond", "define", "else", "lambda", "let", "if", "quote"]

mainLoop :: EnvCtx -> IO ()
mainLoop env = do
    let settings = replSettings $ validWords env
    runInputT settings (rep env) >>= \case
        Just env'   -> mainLoop env'
        Nothing     -> exitSuccess

rep :: EnvCtx -> Repl (Maybe EnvCtx)
rep env = getInputLine "repl> " >>= \case
    Nothing     -> outputStrLn "Goodbye." >> return Nothing
    Just input  -> case dropWhile isSpace input of
                       "" -> rep env
                       i  -> liftIO (process env i) >>= return . Just

process :: EnvCtx -> String -> IO EnvCtx
process env str = do
    res <- safeExec $ execText env $ T.pack str
    case res of
        Left err   -> putStrLn err >> return env
        Right env' -> return env'
