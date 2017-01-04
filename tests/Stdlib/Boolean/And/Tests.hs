{-# LANGUAGE OverloadedStrings #-}

module Stdlib.Boolean.And.Tests(tests)
 where

import qualified Data.Text as T
import           Test.Tasty(TestTree, testGroup)

import Stdlib.Run(mkTests)

testData :: [(FilePath, T.Text)]
testData = [
    ("tests/Stdlib/Boolean/And/and-01.scm", "#t"),
    ("tests/Stdlib/Boolean/And/and-02.scm", "#t"),
    ("tests/Stdlib/Boolean/And/and-03.scm", "#f")
 ]

tests :: TestTree
tests = testGroup "and" $ mkTests testData
