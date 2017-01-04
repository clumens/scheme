{-# LANGUAGE OverloadedStrings #-}

module Stdlib.List.List.Tests(tests)
 where

import qualified Data.Text as T
import           Test.Tasty(TestTree, testGroup)

import Stdlib.Run(mkTests)

testData :: [(FilePath, T.Text)]
testData = [
    ("tests/Stdlib/List/List/list-01.scm", "()"),
    ("tests/Stdlib/List/List/list-02.scm", "(a 3 c)")
 ]

tests :: TestTree
tests = testGroup "list" $ mkTests testData
