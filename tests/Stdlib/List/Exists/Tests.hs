{-# LANGUAGE OverloadedStrings #-}

module Stdlib.List.Exists.Tests(tests)
 where

import qualified Data.Text as T
import           Test.Tasty(TestTree, testGroup)

import Stdlib.Run(mkTests)

testData :: [(FilePath, T.Text)]
testData = [
    ("tests/Stdlib/List/Exists/exists-01.scm", "#f"),
    ("tests/Stdlib/List/Exists/exists-02.scm", "#t"),
    ("tests/Stdlib/List/Exists/exists-03.scm", "#f")
 ]

tests :: TestTree
tests = testGroup "exists" $ mkTests testData
