{-# LANGUAGE OverloadedStrings #-}

module Stdlib.Boolean.Or.Tests(tests)
 where

import qualified Data.Text as T
import           Test.Tasty(TestTree, testGroup)

import Stdlib.Run(mkTests)

testData :: [(FilePath, T.Text)]
testData = [
    ("tests/Stdlib/Boolean/Or/or-01.scm", "#f"),
    ("tests/Stdlib/Boolean/Or/or-02.scm", "#f"),
    ("tests/Stdlib/Boolean/Or/or-03.scm", "#t")
 ]

tests :: TestTree
tests = testGroup "or" $ mkTests testData
