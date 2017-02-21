{-# LANGUAGE OverloadedStrings #-}

module Stdlib.List.ListTail.Tests(tests)
 where

import qualified Data.Text as T
import           Test.Tasty(TestTree, testGroup)

import Stdlib.Run(mkTests)

testData :: [(FilePath, T.Text)]
testData = [
    ("tests/Stdlib/List/ListTail/list-tail-01.scm", "(a b c d)"),
    ("tests/Stdlib/List/ListTail/list-tail-02.scm", "()"),
    ("tests/Stdlib/List/ListTail/list-tail-03.scm", "Error (list-error):\n\tError:       index out of bounds\n\tIn function: list-tail"),
    ("tests/Stdlib/List/ListTail/list-tail-04.scm", "Error (list-error):\n\tError:       index out of bounds\n\tIn function: list-tail"),
    ("tests/Stdlib/List/ListTail/list-tail-05.scm", "()"),
    ("tests/Stdlib/List/ListTail/list-tail-06.scm", "Error (type-error):\n\tExpected: List\n\tGot:      Atom\n\tIn value: abcd")
 ]

tests :: TestTree
tests = testGroup "list-tail" $ mkTests testData
