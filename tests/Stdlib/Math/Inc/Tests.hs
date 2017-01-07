{-# LANGUAGE OverloadedStrings #-}

module Stdlib.Math.Inc.Tests(tests)
 where

import qualified Data.Text as T
import           Test.Tasty(TestTree, testGroup)

import Stdlib.Run(mkTests)

testData :: [(FilePath, T.Text)]
testData = [
    ("tests/Stdlib/Math/Inc/inc-01.scm", "0"),
    ("tests/Stdlib/Math/Inc/inc-02.scm", "11"),
    ("tests/Stdlib/Math/Inc/inc-03.scm", "EXCEPTION")
 ]

tests :: TestTree
tests = testGroup "inc" $ mkTests testData
