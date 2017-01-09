{-# LANGUAGE OverloadedStrings #-}

module Stdlib.Number.Positive.Tests(tests)
 where

import qualified Data.Text as T
import           Test.Tasty(TestTree, testGroup)

import Stdlib.Run(mkTests)

testData :: [(FilePath, T.Text)]
testData = [
    ("tests/Stdlib/Number/Positive/positive-01.scm", "#f"),
    ("tests/Stdlib/Number/Positive/positive-02.scm", "#f"),
    ("tests/Stdlib/Number/Positive/positive-03.scm", "#t"),
    ("tests/Stdlib/Number/Positive/positive-04.scm", "Error (type-error):\n\tExpected: Number\n\tGot:      Atom\n\tIn value: a")
 ]

tests :: TestTree
tests = testGroup "positive" $ mkTests testData
