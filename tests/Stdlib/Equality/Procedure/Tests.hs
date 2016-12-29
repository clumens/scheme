{-# LANGUAGE OverloadedStrings #-}

module Stdlib.Equality.Procedure.Tests(tests)
 where

import qualified Data.Text as T
import           Test.Tasty(TestTree, testGroup)

import Stdlib.Run(mkTests)

testData :: [(FilePath, T.Text)]
testData = [
    ("tests/Stdlib/Equality/Procedure/procedure-01.scm", "#f"),
    ("tests/Stdlib/Equality/Procedure/procedure-02.scm", "#f"),
    ("tests/Stdlib/Equality/Procedure/procedure-03.scm", "#f"),
    ("tests/Stdlib/Equality/Procedure/procedure-04.scm", "#f"),
    ("tests/Stdlib/Equality/Procedure/procedure-05.scm", "#t"),
    ("tests/Stdlib/Equality/Procedure/procedure-06.scm", "#f"),
    ("tests/Stdlib/Equality/Procedure/procedure-07.scm", "#f"),
    ("tests/Stdlib/Equality/Procedure/procedure-08.scm", "#f")
 ]

tests :: TestTree
tests = testGroup "procedure" $ mkTests testData
