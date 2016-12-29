{-# LANGUAGE OverloadedStrings #-}

module Stdlib.Equality.String.Tests(tests)
 where

import qualified Data.Text as T
import           Test.Tasty(TestTree, testGroup)

import Stdlib.Run(mkTests)

testData :: [(FilePath, T.Text)]
testData = [
    ("tests/Stdlib/Equality/String/string-01.scm", "#f"),
    ("tests/Stdlib/Equality/String/string-02.scm", "#f"),
    ("tests/Stdlib/Equality/String/string-03.scm", "#f"),
    ("tests/Stdlib/Equality/String/string-04.scm", "#f"),
    ("tests/Stdlib/Equality/String/string-05.scm", "#f"),
    ("tests/Stdlib/Equality/String/string-06.scm", "#t"),
    ("tests/Stdlib/Equality/String/string-07.scm", "#f"),
    ("tests/Stdlib/Equality/String/string-08.scm", "#f")
 ]

tests :: TestTree
tests = testGroup "string" $ mkTests testData
