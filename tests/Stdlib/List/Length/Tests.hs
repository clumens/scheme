{-# LANGUAGE OverloadedStrings #-}

module Stdlib.List.Length.Tests(tests)
 where

import qualified Data.Text as T
import           Test.Tasty(TestTree, testGroup)

import Stdlib.Run(mkTests)

testData :: [(FilePath, T.Text)]
testData = [
    ("tests/Stdlib/List/Length/length-01.scm", "3"),
    ("tests/Stdlib/List/Length/length-02.scm", "3"),
    ("tests/Stdlib/List/Length/length-03.scm", "0")
 ]

tests :: TestTree
tests = testGroup "length" $ mkTests testData
