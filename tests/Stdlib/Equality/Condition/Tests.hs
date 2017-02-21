{-# LANGUAGE OverloadedStrings #-}

module Stdlib.Equality.Condition.Tests(tests)
 where

import qualified Data.Text as T
import           Test.Tasty(TestTree, testGroup)

import Stdlib.Run(mkTests)

testData :: [(FilePath, T.Text)]
testData = [
    ("tests/Stdlib/Equality/Condition/condition-01.scm", "#f"),
    ("tests/Stdlib/Equality/Condition/condition-02.scm", "#f"),
    ("tests/Stdlib/Equality/Condition/condition-03.scm", "#f"),
    ("tests/Stdlib/Equality/Condition/condition-04.scm", "#f"),
    ("tests/Stdlib/Equality/Condition/condition-05.scm", "#f"),
    ("tests/Stdlib/Equality/Condition/condition-06.scm", "#f"),
    ("tests/Stdlib/Equality/Condition/condition-07.scm", "#f"),
    ("tests/Stdlib/Equality/Condition/condition-08.scm", "#f"),
    ("tests/Stdlib/Equality/Condition/condition-09.scm", "#t")
 ]

tests :: TestTree
tests = testGroup "condition" $ mkTests testData
