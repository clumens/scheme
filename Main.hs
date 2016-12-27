{-# LANGUAGE OverloadedStrings #-}

import           Control.Conditional(whenM)
import           Control.Monad(void)
import qualified Data.Text.IO as TIO
import           System.Directory(doesFileExist)
import           System.Environment(getArgs)

import Scheme.Eval(basicEnv, evalFile, evalText, safeExec)
import Scheme.Repl(mainLoop)

main :: IO ()
main = do
    args <- getArgs

    contents <- TIO.readFile "library.scm"
    env <- evalFile basicEnv contents

    if (length args > 0)
    then mapM_ (\arg -> whenM (doesFileExist arg) $ do
                            s <- TIO.readFile arg
                            void $ safeExec $ evalText env s)
               args
    else mainLoop env
