{-# LANGUAGE OverloadedStrings #-}

module Stdlib.Boolean.Not.Tests(tests)
 where

import qualified Data.Text as T
import           Test.Tasty(TestTree, testGroup)

import Stdlib.Run(mkTests)

testData :: [(FilePath, T.Text)]
testData = [
    ("tests/Stdlib/Boolean/Not/not-01.scm", "#f"),
    ("tests/Stdlib/Boolean/Not/not-02.scm", "#f"),
    ("tests/Stdlib/Boolean/Not/not-03.scm", "#t"),
    ("tests/Stdlib/Boolean/Not/not-04.scm", "#f"),
    ("tests/Stdlib/Boolean/Not/not-05.scm", "#f")
 ]

tests :: TestTree
tests = testGroup "not" $ mkTests testData
