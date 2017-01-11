{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Scheme.Repl(mainLoop)
 where

import Scheme.Environment(environmentWords)
import Scheme.Eval(evalText)
import Scheme.Exceptions(catchHaskellExceptions, defaultExceptionHandler)
import Scheme.LispVal(LispVal(Condition), SchemeSt(..), showVal)

import           Control.Monad.IO.Class(liftIO)
import           Data.Char(isSpace)
import           Data.List(sort)
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

validWords :: SchemeSt -> [T.Text]
validWords state = sort $
    environmentWords (stBindings state) ++
    -- Add special words defined in Eval.hs.
    ["apply", "begin", "cond", "define", "define-condition-type", "else", "guard", "if", "lambda", "let", "quote"]

mainLoop :: SchemeSt -> IO ()
mainLoop state = do
    let settings = replSettings $ validWords state
    runInputT settings (rep state) >>= \case
        Just state' -> mainLoop state'
        Nothing     -> exitSuccess

rep :: SchemeSt -> Repl (Maybe SchemeSt)
rep state = getInputLine "repl> " >>= \case
    Nothing     -> return Nothing
    Just input  -> case dropWhile isSpace input of
                       "" -> rep state
                       i  -> liftIO (process state i) >>= return . Just

process :: SchemeSt-> String -> IO SchemeSt
process state str = do
    (ret, state') <- catchHaskellExceptions $ evalText state $ T.pack str
    case ret of
        Condition _ _ -> defaultExceptionHandler ret
        result        -> TIO.putStrLn $ showVal result

    return state'
