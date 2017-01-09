{-# LANGUAGE OverloadedStrings #-}

module Stdlib.Number.Negative.Tests(tests)
 where

import qualified Data.Text as T
import           Test.Tasty(TestTree, testGroup)

import Stdlib.Run(mkTests)

testData :: [(FilePath, T.Text)]
testData = [
    ("tests/Stdlib/Number/Negative/negative-01.scm", "#t"),
    ("tests/Stdlib/Number/Negative/negative-02.scm", "#f"),
    ("tests/Stdlib/Number/Negative/negative-03.scm", "#f"),
    ("tests/Stdlib/Number/Negative/negative-04.scm", "Error (type-error):\n\tExpected: Number\n\tGot:      Character\n\tIn value: x")
 ]

tests :: TestTree
tests = testGroup "negative" $ mkTests testData
