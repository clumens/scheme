{-# LANGUAGE OverloadedStrings #-}

module Stdlib.List.Memp.Tests(tests)
 where

import qualified Data.Text as T
import           Test.Tasty(TestTree, testGroup)

import Stdlib.Run(mkTests)

testData :: [(FilePath, T.Text)]
testData = [
    ("tests/Stdlib/List/Memp/memp-01.scm", "(4 1 5 9 2 6 5)"),
    ("tests/Stdlib/List/Memp/memp-02.scm", "#f")
 ]

tests :: TestTree
tests = testGroup "memp" $ mkTests testData
