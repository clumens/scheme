{-# LANGUAGE OverloadedStrings #-}

module Stdlib.Control.When.Tests(tests)
 where

import qualified Data.Text as T
import           Test.Tasty(TestTree, testGroup)

import Stdlib.Run(mkTests)

testData :: [(FilePath, T.Text)]
testData = [
    ("tests/Stdlib/Control/When/when-01.scm", "greater"),
    ("tests/Stdlib/Control/When/when-02.scm", "#f"),
    ("tests/Stdlib/Control/When/when-03.scm", "Error (type-error):\n\tExpected: Bool\n\tGot:      Number\n\tIn value: 3")
 ]

tests :: TestTree
tests = testGroup "when" $ mkTests testData
