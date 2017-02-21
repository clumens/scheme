{-# LANGUAGE OverloadedStrings #-}

module Stdlib.List.Find.Tests(tests)
 where

import qualified Data.Text as T
import           Test.Tasty(TestTree, testGroup)

import Stdlib.Run(mkTests)

testData :: [(FilePath, T.Text)]
testData = [
    ("tests/Stdlib/List/Find/find-01.scm", "4"),
    ("tests/Stdlib/List/Find/find-02.scm", "#f"),
    ("tests/Stdlib/List/Find/find-03.scm", "#f")
 ]

tests :: TestTree
tests = testGroup "find" $ mkTests testData
