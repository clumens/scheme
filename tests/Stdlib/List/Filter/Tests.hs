{-# LANGUAGE OverloadedStrings #-}

module Stdlib.List.Filter.Tests(tests)
 where

import qualified Data.Text as T
import           Test.Tasty(TestTree, testGroup)

import Stdlib.Run(mkTests)

testData :: [(FilePath, T.Text)]
testData = [
    ("tests/Stdlib/List/Filter/filter-01.scm", "(4 2 6)"),
    ("tests/Stdlib/List/Filter/filter-02.scm", "()"),
    ("tests/Stdlib/List/Filter/filter-03.scm", "Error (type-error):\n\tExpected: List\n\tGot:      Bool\n\tIn value: #t"),
    ("tests/Stdlib/List/Filter/filter-04.scm", "Error (type-error):\n\tExpected: Function\n\tGot:      Atom\n\tIn value: fn")
 ]

tests :: TestTree
tests = testGroup "filter" $ mkTests testData
