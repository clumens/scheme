{-# LANGUAGE OverloadedStrings #-}

module Stdlib.List.Remp.Tests(tests)
 where

import qualified Data.Text as T
import           Test.Tasty(TestTree, testGroup)

import Stdlib.Run(mkTests)

testData :: [(FilePath, T.Text)]
testData = [
    ("tests/Stdlib/List/Remp/remp-01.scm", "(3 1 1 5 9 5)"),
    ("tests/Stdlib/List/Remp/remp-02.scm", "()")
 ]

tests :: TestTree
tests = testGroup "remp" $ mkTests testData
