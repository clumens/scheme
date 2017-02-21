{-# LANGUAGE OverloadedStrings #-}

module Stdlib.List.Remq.Tests(tests)
 where

import qualified Data.Text as T
import           Test.Tasty(TestTree, testGroup)

import Stdlib.Run(mkTests)

testData :: [(FilePath, T.Text)]
testData = [
    ("tests/Stdlib/List/Remq/remq-01.scm", "(b c)"),
    ("tests/Stdlib/List/Remq/remq-02.scm", "(a c)"),
    ("tests/Stdlib/List/Remq/remq-03.scm", "(b c d)"),
    ("tests/Stdlib/List/Remq/remq-04.scm", "()")
 ]

tests :: TestTree
tests = testGroup "remq" $ mkTests testData
