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
    ("tests/Stdlib/Number/Zero/zero-04.scm", "EXCEPTION")
 ]

tests :: TestTree
tests = testGroup "zero" $ mkTests testData
