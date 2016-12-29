{-# LANGUAGE OverloadedStrings #-}

module Stdlib.Equality.Character.Tests(tests)
 where

import qualified Data.Text as T
import           Test.Tasty(TestTree, testGroup)

import Stdlib.Run(mkTests)

testData :: [(FilePath, T.Text)]
testData = [
    ("tests/Stdlib/Equality/Character/character-01.scm", "#f"),
    ("tests/Stdlib/Equality/Character/character-02.scm", "#f"),
    ("tests/Stdlib/Equality/Character/character-03.scm", "#f"),
    ("tests/Stdlib/Equality/Character/character-04.scm", "#f"),
    ("tests/Stdlib/Equality/Character/character-05.scm", "#f"),
    ("tests/Stdlib/Equality/Character/character-06.scm", "#f"),
    ("tests/Stdlib/Equality/Character/character-07.scm", "#f"),
    ("tests/Stdlib/Equality/Character/character-08.scm", "#t")
 ]

tests :: TestTree
tests = testGroup "character" $ mkTests testData
