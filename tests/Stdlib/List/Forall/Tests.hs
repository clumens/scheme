{-# LANGUAGE OverloadedStrings #-}

module Stdlib.List.Forall.Tests(tests)
 where

import qualified Data.Text as T
import           Test.Tasty(TestTree, testGroup)

import Stdlib.Run(mkTests)

testData :: [(FilePath, T.Text)]
testData = [
    ("tests/Stdlib/List/Forall/forall-01.scm", "#t"),
    ("tests/Stdlib/List/Forall/forall-02.scm", "#f"),
    ("tests/Stdlib/List/Forall/forall-03.scm", "#t")
 ]

tests :: TestTree
tests = testGroup "for-all" $ mkTests testData
