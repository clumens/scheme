{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.IO as TIO

import Eval(basicEnv, evalText)
import Repl(mainLoop)

main :: IO ()
main = do
    contents <- TIO.readFile "library.scm"
    env <- evalText basicEnv contents
    mainLoop env
