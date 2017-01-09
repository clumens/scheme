{-# LANGUAGE OverloadedStrings #-}

module Stdlib.Number.Even.Tests(tests)
 where

import qualified Data.Text as T
import           Test.Tasty(TestTree, testGroup)

import Stdlib.Run(mkTests)

testData :: [(FilePath, T.Text)]
testData = [
    ("tests/Stdlib/Number/Even/even-01.scm", "#t"),
    ("tests/Stdlib/Number/Even/even-02.scm", "#t"),
    ("tests/Stdlib/Number/Even/even-03.scm", "#t"),
    ("tests/Stdlib/Number/Even/even-04.scm", "#f"),
    ("tests/Stdlib/Number/Even/even-05.scm", "Error (type-error):\n\tExpected: Number\n\tGot:      List\n\tIn value: (1 2 3)")
 ]

tests :: TestTree
tests = testGroup "even" $ mkTests testData
