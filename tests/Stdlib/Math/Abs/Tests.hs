{-# LANGUAGE OverloadedStrings #-}

module Stdlib.Math.Abs.Tests(tests)
 where

import qualified Data.Text as T
import           Test.Tasty(TestTree, testGroup)

import Stdlib.Run(mkTests)

testData :: [(FilePath, T.Text)]
testData = [
    ("tests/Stdlib/Math/Abs/abs-01.scm", "100"),
    ("tests/Stdlib/Math/Abs/abs-02.scm", "100"),
    ("tests/Stdlib/Math/Abs/abs-03.scm", "0"),
    ("tests/Stdlib/Math/Abs/abs-04.scm", "EXCEPTION")
 ]

tests :: TestTree
tests = testGroup "abs" $ mkTests testData
