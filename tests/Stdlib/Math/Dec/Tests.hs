{-# LANGUAGE OverloadedStrings #-}

module Stdlib.Math.Dec.Tests(tests)
 where

import qualified Data.Text as T
import           Test.Tasty(TestTree, testGroup)

import Stdlib.Run(mkTests)

testData :: [(FilePath, T.Text)]
testData = [
    ("tests/Stdlib/Math/Dec/dec-01.scm", "0"),
    ("tests/Stdlib/Math/Dec/dec-02.scm", "-11"),
    ("tests/Stdlib/Math/Dec/dec-03.scm", "EXCEPTION")
 ]

tests :: TestTree
tests = testGroup "dec" $ mkTests testData
