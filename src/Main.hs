{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.IO as TIO

import Eval(basicEnv, evalFile)
import Repl(mainLoop)

main :: IO ()
main = do
    contents <- TIO.readFile "library.scm"
    env <- evalFile basicEnv contents
    mainLoop env
