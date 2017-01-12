{-# LANGUAGE OverloadedStrings #-}

module Stdlib.List.Cdr.Tests(tests)
 where

import qualified Data.Text as T
import           Test.Tasty(TestTree, testGroup)

import Stdlib.Run(mkTests)

testData :: [(FilePath, T.Text)]
testData = [
    ("tests/Stdlib/List/Cdr/cdr-01.scm", "(b c d)"),
    ("tests/Stdlib/List/Cdr/cdr-02.scm", "Error (list-error):\n\tEmpty list encountered in function: cdr"),
    ("tests/Stdlib/List/Cdr/cdr-03.scm", "()")
 ]

tests :: TestTree
tests = testGroup "cdr" $ mkTests testData
