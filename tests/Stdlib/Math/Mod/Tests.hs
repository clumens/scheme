{-# LANGUAGE OverloadedStrings #-}

module Stdlib.Math.Mod.Tests(tests)
 where

import qualified Data.Text as T
import           Test.Tasty(TestTree, testGroup)

import Stdlib.Run(mkTests)

testData :: [(FilePath, T.Text)]
testData = [
    ("tests/Stdlib/Math/Mod/mod-01.scm", "3"),
    ("tests/Stdlib/Math/Mod/mod-02.scm", "0"),
    ("tests/Stdlib/Math/Mod/mod-03.scm", "4"),
    ("tests/Stdlib/Math/Mod/mod-04.scm", "0"),
    ("tests/Stdlib/Math/Mod/mod-05.scm", "Error (div-by-zero-error):\n\tDivide by zero")
 ]

tests :: TestTree
tests = testGroup "mod" $ mkTests testData
