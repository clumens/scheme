{-# LANGUAGE OverloadedStrings #-}

module Stdlib.Equality.Boolean.Tests(tests)
 where

import qualified Data.Text as T
import           Test.Tasty(TestTree, testGroup)

import Stdlib.Run(mkTests)

testData :: [(FilePath, T.Text)]
testData = [
    ("tests/Stdlib/Equality/Boolean/boolean-01.scm", "#t"),
    ("tests/Stdlib/Equality/Boolean/boolean-02.scm", "#f"),
    ("tests/Stdlib/Equality/Boolean/boolean-03.scm", "#f"),
    ("tests/Stdlib/Equality/Boolean/boolean-04.scm", "#f"),
    ("tests/Stdlib/Equality/Boolean/boolean-05.scm", "#f"),
    ("tests/Stdlib/Equality/Boolean/boolean-06.scm", "#f"),
    ("tests/Stdlib/Equality/Boolean/boolean-07.scm", "#f"),
    ("tests/Stdlib/Equality/Boolean/boolean-08.scm", "#f"),
    ("tests/Stdlib/Equality/Boolean/boolean-09.scm", "#t"),
    ("tests/Stdlib/Equality/Boolean/boolean-10.scm", "#t"),
    ("tests/Stdlib/Equality/Boolean/boolean-11.scm", "#f")
 ]

tests :: TestTree
tests = testGroup "boolean" $ mkTests testData
