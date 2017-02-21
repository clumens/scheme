{-# LANGUAGE OverloadedStrings #-}

module Stdlib.String.StringToList.Tests(tests)
 where

import qualified Data.Text as T
import           Test.Tasty(TestTree, testGroup)

import Stdlib.Run(mkTests)

testData :: [(FilePath, T.Text)]
testData = [
    ("tests/Stdlib/String/StringToList/string2list-01.scm", "()"),
    ("tests/Stdlib/String/StringToList/string2list-02.scm", "( )"),
    ("tests/Stdlib/String/StringToList/string2list-03.scm", "(a b c)"),
    ("tests/Stdlib/String/StringToList/string2list-04.scm", "Error (type-error):\n\tExpected: String\n\tGot:      List\n\tIn value: (a b c)")
 ]

tests :: TestTree
tests = testGroup "string->list" $ mkTests testData
