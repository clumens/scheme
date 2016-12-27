{-# LANGUAGE OverloadedStrings #-}

module Stdlib.Equality.List.Tests(tests)
 where

import qualified Data.Text as T
import           Test.Tasty(TestTree, testGroup)

import Stdlib.Run(mkTests)

testData :: [(FilePath, T.Text)]
testData = [
    ("tests/Stdlib/Equality/List/list-01.scm", "#f"),
    ("tests/Stdlib/Equality/List/list-02.scm", "#t"),
    ("tests/Stdlib/Equality/List/list-03.scm", "#t"),
    ("tests/Stdlib/Equality/List/list-04.scm", "#f"),
    ("tests/Stdlib/Equality/List/list-05.scm", "#f"),
    ("tests/Stdlib/Equality/List/list-06.scm", "#f"),
    ("tests/Stdlib/Equality/List/list-07.scm", "#f")
 ]

tests :: TestTree
tests = testGroup "list" $ mkTests testData
