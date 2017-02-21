{-# LANGUAGE OverloadedStrings #-}

module Stdlib.Math.Min.Tests(tests)
 where

import qualified Data.Text as T
import           Test.Tasty(TestTree, testGroup)

import Stdlib.Run(mkTests)

testData :: [(FilePath, T.Text)]
testData = [
    ("tests/Stdlib/Math/Min/min-01.scm", "1"),
    ("tests/Stdlib/Math/Min/min-02.scm", "1"),
    ("tests/Stdlib/Math/Min/min-03.scm", "1.1"),
    ("tests/Stdlib/Math/Min/min-04.scm", "Error (syntax-error):\n\texpected at least two arguments"),
    ("tests/Stdlib/Math/Min/min-05.scm", "Error (type-error):\n\tExpected: Number\n\tGot:      Bool\n\tIn value: #t")
 ]

tests :: TestTree
tests = testGroup "min" $ mkTests testData
