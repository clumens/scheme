{-# LANGUAGE OverloadedStrings #-}

import           Control.Conditional(whenM)
import           Control.Monad(void)
import qualified Data.Text.IO as TIO
import           System.Directory(doesFileExist)
import           System.Environment(getArgs)

import Scheme.Eval(basicEnv, execFile, execText, safeExec)
import Scheme.Repl(mainLoop)

main :: IO ()
main = do
    args <- getArgs

    contents <- TIO.readFile "library.scm"
    env <- execFile basicEnv contents

    if not (null args)
    then mapM_ (\arg -> whenM (doesFileExist arg) $ do
                            s <- TIO.readFile arg
                            void $ safeExec $ execText env s)
               args
    else mainLoop env
