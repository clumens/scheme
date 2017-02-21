{-# LANGUAGE OverloadedStrings #-}

module Stdlib.List.ListToString.Tests(tests)
 where

import qualified Data.Text as T
import           Test.Tasty(TestTree, testGroup)

import Stdlib.Run(mkTests)

testData :: [(FilePath, T.Text)]
testData = [
    ("tests/Stdlib/List/ListToString/list2string-01.scm", "\"\""),
    ("tests/Stdlib/List/ListToString/list2string-02.scm", "\" \""),
    ("tests/Stdlib/List/ListToString/list2string-03.scm", "\"abc\""),
    ("tests/Stdlib/List/ListToString/list2string-04.scm", "Error (type-error):\n\tExpected: List\n\tGot:      Bool\n\tIn value: #t"),
    ("tests/Stdlib/List/ListToString/list2string-05.scm", "Error (type-error):\n\tExpected: Character\n\tGot:      Bool\n\tIn value: #t")
 ]

tests :: TestTree
tests = testGroup "list->string" $ mkTests testData
