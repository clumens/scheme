{-# LANGUAGE OverloadedStrings #-}

module Stdlib.List.Append.Tests(tests)
 where

import qualified Data.Text as T
import           Test.Tasty(TestTree, testGroup)

import Stdlib.Run(mkTests)

testData :: [(FilePath, T.Text)]
testData = [
    ("tests/Stdlib/List/Append/append-01.scm", "(x y)"),
    ("tests/Stdlib/List/Append/append-02.scm", "(a b c d)"),
    ("tests/Stdlib/List/Append/append-03.scm", "(a (b) (c))"),
    ("tests/Stdlib/List/Append/append-04.scm", "(a)"),
    ("tests/Stdlib/List/Append/append-05.scm", "EXCEPTION")
 ]

tests :: TestTree
tests = testGroup "append" $ mkTests testData
