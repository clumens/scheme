{-# LANGUAGE OverloadedStrings #-}

module Stdlib.List.Null.Tests(tests)
 where

import qualified Data.Text as T
import           Test.Tasty(TestTree, testGroup)

import Stdlib.Run(mkTests)

testData :: [(FilePath, T.Text)]
testData = [
    ("tests/Stdlib/List/Null/null-01.scm", "#t"),
    ("tests/Stdlib/List/Null/null-02.scm", "#f"),
    ("tests/Stdlib/List/Null/null-03.scm", "#f"),
    ("tests/Stdlib/List/Null/null-04.scm", "#f"),
    ("tests/Stdlib/List/Null/null-05.scm", "#f"),
    ("tests/Stdlib/List/Null/null-06.scm", "#f")
 ]

tests :: TestTree
tests = testGroup "null" $ mkTests testData
