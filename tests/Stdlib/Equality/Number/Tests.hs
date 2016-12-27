{-# LANGUAGE OverloadedStrings #-}

module Stdlib.Equality.Number.Tests(tests)
 where

import qualified Data.Text as T
import           Test.Tasty(TestTree, testGroup)

import Stdlib.Run(mkTests)

testData :: [(FilePath, T.Text)]
testData = [
    ("tests/Stdlib/Equality/Number/number-01.scm", "#f"),
    ("tests/Stdlib/Equality/Number/number-02.scm", "#f"),
    ("tests/Stdlib/Equality/Number/number-03.scm", "#f"),
    ("tests/Stdlib/Equality/Number/number-04.scm", "#t"),
    ("tests/Stdlib/Equality/Number/number-05.scm", "#f"),
    ("tests/Stdlib/Equality/Number/number-06.scm", "#f"),
    ("tests/Stdlib/Equality/Number/number-07.scm", "#t")
 ]

tests :: TestTree
tests = testGroup "number" $ mkTests testData
