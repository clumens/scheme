{-# LANGUAGE OverloadedStrings #-}

module Stdlib.List.Cons.Tests(tests)
 where

import qualified Data.Text as T
import           Test.Tasty(TestTree, testGroup)

import Stdlib.Run(mkTests)

testData :: [(FilePath, T.Text)]
testData = [
    ("tests/Stdlib/List/Cons/cons-01.scm", "(a)"),
    ("tests/Stdlib/List/Cons/cons-02.scm", "((a) b c d)"),
    ("tests/Stdlib/List/Cons/cons-03.scm", "(\"a\" b c)")
 ]

tests :: TestTree
tests = testGroup "cons" $ mkTests testData
