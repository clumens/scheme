{-# LANGUAGE OverloadedStrings #-}

module Stdlib.Math.Div.Tests(tests)
 where

import qualified Data.Text as T
import           Test.Tasty(TestTree, testGroup)

import Stdlib.Run(mkTests)

testData :: [(FilePath, T.Text)]
testData = [
    ("tests/Stdlib/Math/Div/div-01.scm", "3"),
    ("tests/Stdlib/Math/Div/div-02.scm", "4"),
    ("tests/Stdlib/Math/Div/div-03.scm", "-1"),
    ("tests/Stdlib/Math/Div/div-04.scm", "-3"),
    ("tests/Stdlib/Math/Div/div-05.scm", "EXCEPTION")
 ]

tests :: TestTree
tests = testGroup "div" $ mkTests testData
