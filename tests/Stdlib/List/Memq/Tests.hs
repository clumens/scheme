{-# LANGUAGE OverloadedStrings #-}

module Stdlib.List.Memq.Tests(tests)
 where

import qualified Data.Text as T
import           Test.Tasty(TestTree, testGroup)

import Stdlib.Run(mkTests)

testData :: [(FilePath, T.Text)]
testData = [
    ("tests/Stdlib/List/Memq/memq-01.scm", "(a b c)"),
    ("tests/Stdlib/List/Memq/memq-02.scm", "(b c)"),
    ("tests/Stdlib/List/Memq/memq-03.scm", "#f"),
    ("tests/Stdlib/List/Memq/memq-04.scm", "#f"),
    ("tests/Stdlib/List/Memq/memq-05.scm", "#f")
 ]

tests :: TestTree
tests = testGroup "memq" $ mkTests testData
