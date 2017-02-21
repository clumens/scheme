{-# LANGUAGE OverloadedStrings #-}

module Stdlib.List.Map.Tests(tests)
 where

import qualified Data.Text as T
import           Test.Tasty(TestTree, testGroup)

import Stdlib.Run(mkTests)

testData :: [(FilePath, T.Text)]
testData = [
    ("tests/Stdlib/List/Map/map-01.scm", "(b e h)"),
    ("tests/Stdlib/List/Map/map-02.scm", "(1 4 9 16 25)"),
    ("tests/Stdlib/List/Map/map-03.scm", "Error (type-error):\n\tExpected: List\n\tGot:      Bool\n\tIn value: #t"),
    ("tests/Stdlib/List/Map/map-04.scm", "Error (type-error):\n\tExpected: Function\n\tGot:      Atom\n\tIn value: fn")
 ]

tests :: TestTree
tests = testGroup "map" $ mkTests testData
