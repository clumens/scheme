{-# LANGUAGE OverloadedStrings #-}

module Stdlib.Math.Max.Tests(tests)
 where

import qualified Data.Text as T
import           Test.Tasty(TestTree, testGroup)

import Stdlib.Run(mkTests)

testData :: [(FilePath, T.Text)]
testData = [
    ("tests/Stdlib/Math/Max/max-01.scm", "4"),
    ("tests/Stdlib/Math/Max/max-02.scm", "4"),
    ("tests/Stdlib/Math/Max/max-03.scm", "4.4"),
    ("tests/Stdlib/Math/Max/max-04.scm", "Error (syntax-error):\n\texpected at least two arguments"),
    ("tests/Stdlib/Math/Max/max-05.scm", "Error (type-error):\n\tExpected: Number\n\tGot:      Bool\n\tIn value: #t")
 ]

tests :: TestTree
tests = testGroup "max" $ mkTests testData
