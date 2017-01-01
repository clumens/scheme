{-# LANGUAGE OverloadedStrings #-}

module Stdlib.Control.Unless.Tests(tests)
 where

import qualified Data.Text as T
import           Test.Tasty(TestTree, testGroup)

import Stdlib.Run(mkTests)

testData :: [(FilePath, T.Text)]
testData = [
    ("tests/Stdlib/Control/Unless/unless-01.scm", "#f"),
    ("tests/Stdlib/Control/Unless/unless-02.scm", "less"),
    ("tests/Stdlib/Control/Unless/unless-03.scm", "EXCEPTION")
 ]

tests :: TestTree
tests = testGroup "unless" $ mkTests testData
