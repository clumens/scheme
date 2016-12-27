module Main where

import Test.Tasty(defaultMain, testGroup)

import qualified Stdlib.List.Reverse.Tests
import qualified Stdlib.Boolean.Not.Tests

main :: IO ()
main = defaultMain $ testGroup "Tests"
    [ Stdlib.List.Reverse.Tests.tests,
      Stdlib.Boolean.Not.Tests.tests ]
