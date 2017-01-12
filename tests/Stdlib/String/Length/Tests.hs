{-# LANGUAGE OverloadedStrings #-}

module Stdlib.String.Length.Tests(tests)
 where

import qualified Data.Text as T
import           Test.Tasty(TestTree, testGroup)

import Stdlib.Run(mkTests)

testData :: [(FilePath, T.Text)]
testData = [
    ("tests/Stdlib/String/Length/length-01.scm", "0"),
    ("tests/Stdlib/String/Length/length-02.scm", "3"),
    ("tests/Stdlib/String/Length/length-03.scm", "Error (type-error):\n\tExpected: String\n\tGot:      List\n\tIn value: ()")
 ]

tests :: TestTree
tests = testGroup "string-length" $ mkTests testData
