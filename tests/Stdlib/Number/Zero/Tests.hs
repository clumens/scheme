{-# LANGUAGE OverloadedStrings #-}

module Stdlib.Number.Zero.Tests(tests)
 where

import qualified Data.Text as T
import           Test.Tasty(TestTree, testGroup)

import Stdlib.Run(mkTests)

testData :: [(FilePath, T.Text)]
testData = [
    ("tests/Stdlib/Number/Zero/zero-01.scm", "#f"),
    ("tests/Stdlib/Number/Zero/zero-02.scm", "#t"),
    ("tests/Stdlib/Number/Zero/zero-03.scm", "#f"),
    ("tests/Stdlib/Number/Zero/zero-04.scm", "Error (type-error):\n\tExpected: Number\n\tGot:      List\n\tIn value: ()")
 ]

tests :: TestTree
tests = testGroup "zero" $ mkTests testData
