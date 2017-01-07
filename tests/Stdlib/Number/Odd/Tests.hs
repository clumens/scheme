{-# LANGUAGE OverloadedStrings #-}

module Stdlib.Number.Odd.Tests(tests)
 where

import qualified Data.Text as T
import           Test.Tasty(TestTree, testGroup)

import Stdlib.Run(mkTests)

testData :: [(FilePath, T.Text)]
testData = [
    ("tests/Stdlib/Number/Odd/odd-01.scm", "#t"),
    ("tests/Stdlib/Number/Odd/odd-02.scm", "#t"),
    ("tests/Stdlib/Number/Odd/odd-03.scm", "#f"),
    ("tests/Stdlib/Number/Odd/odd-04.scm", "#f"),
    ("tests/Stdlib/Number/Odd/odd-05.scm", "EXCEPTION")
 ]

tests :: TestTree
tests = testGroup "odd" $ mkTests testData
