{-# LANGUAGE OverloadedStrings #-}

module Stdlib.List.Car.Tests(tests)
 where

import qualified Data.Text as T
import           Test.Tasty(TestTree, testGroup)

import Stdlib.Run(mkTests)

testData :: [(FilePath, T.Text)]
testData = [
    ("tests/Stdlib/List/Car/car-01.scm", "a"),
    ("tests/Stdlib/List/Car/car-02.scm", "(a)"),
    ("tests/Stdlib/List/Car/car-03.scm", "Error (list-error):\n\tError:       empty list\n\tIn function: car"),
    ("tests/Stdlib/List/Car/car-04.scm", "a")
 ]

tests :: TestTree
tests = testGroup "car" $ mkTests testData
