{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Scheme.Repl(mainLoop)
 where

import Scheme.Eval(evalText)
import Scheme.Exceptions(catchHaskellExceptions, defaultExceptionHandler)
import Scheme.LispVal(EnvCtx, LispVal(Error), showVal)

import           Control.Monad.IO.Class(liftIO)
import           Data.Char(isSpace)
import           Data.List(sort)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
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
    ["apply", "begin", "cond", "define", "define-condition-type", "else", "lambda", "let", "if", "quote"]

mainLoop :: EnvCtx -> IO ()
mainLoop env = do
    let settings = replSettings $ validWords env
    runInputT settings (rep env) >>= \case
        Just env'   -> mainLoop env'
        Nothing     -> exitSuccess

rep :: EnvCtx -> Repl (Maybe EnvCtx)
rep env = getInputLine "repl> " >>= \case
    Nothing     -> return Nothing
    Just input  -> case dropWhile isSpace input of
                       "" -> rep env
                       i  -> liftIO (process env i) >>= return . Just

process :: EnvCtx -> String -> IO EnvCtx
process env str = do
    (ret, env') <- catchHaskellExceptions $ evalText env $ T.pack str
    case ret of
        Error _ _   -> defaultExceptionHandler ret
        result      -> TIO.putStrLn $ showVal result

    return env'
