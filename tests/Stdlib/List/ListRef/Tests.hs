{-# LANGUAGE OverloadedStrings #-}

module Stdlib.List.ListRef.Tests(tests)
 where

import qualified Data.Text as T
import           Test.Tasty(TestTree, testGroup)

import Stdlib.Run(mkTests)

testData :: [(FilePath, T.Text)]
testData = [
    ("tests/Stdlib/List/ListRef/list-ref-01.scm", "d"),
    ("tests/Stdlib/List/ListRef/list-ref-02.scm", "a"),
    ("tests/Stdlib/List/ListRef/list-ref-03.scm", "Error (list-error):\n\tError:       index out of bounds\n\tIn function: list-ref"),
    ("tests/Stdlib/List/ListRef/list-ref-04.scm", "Error (list-error):\n\tError:       index out of bounds\n\tIn function: list-ref"),
    ("tests/Stdlib/List/ListRef/list-ref-05.scm", "Error (list-error):\n\tError:       index out of bounds\n\tIn function: list-ref"),
    ("tests/Stdlib/List/ListRef/list-ref-06.scm", "Error (type-error):\n\tExpected: List\n\tGot:      Atom\n\tIn value: abcd")
 ]

tests :: TestTree
tests = testGroup "list-ref" $ mkTests testData
