{-# LANGUAGE OverloadedStrings #-}

module Stdlib.List.Reverse.Tests(tests)
 where

import qualified Data.Text as T
import           Test.Tasty(TestTree, testGroup)

import Stdlib.Run(mkTests)

testData :: [(FilePath, T.Text)]
testData = [
    ("tests/Stdlib/List/Reverse/reverse-01.scm", "()"),
    ("tests/Stdlib/List/Reverse/reverse-02.scm", "(c b a)"),
    ("tests/Stdlib/List/Reverse/reverse-03.scm", "((e (f)) d (b c) a)")
 ]

tests :: TestTree
tests = testGroup "reverse" $ mkTests testData
