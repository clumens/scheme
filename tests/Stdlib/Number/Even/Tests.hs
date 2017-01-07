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
    ("tests/Stdlib/Number/Even/even-05.scm", "EXCEPTION")
 ]

tests :: TestTree
tests = testGroup "even" $ mkTests testData
