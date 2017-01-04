module Main where

import Test.Tasty(defaultMain, testGroup)

import qualified Stdlib.Boolean.And.Tests
import qualified Stdlib.Boolean.Not.Tests
import qualified Stdlib.Boolean.Or.Tests
import qualified Stdlib.Control.Unless.Tests
import qualified Stdlib.Control.When.Tests
import qualified Stdlib.Equality.Boolean.Tests
import qualified Stdlib.Equality.Character.Tests
import qualified Stdlib.Equality.List.Tests
import qualified Stdlib.Equality.Number.Tests
import qualified Stdlib.Equality.Procedure.Tests
import qualified Stdlib.Equality.String.Tests
import qualified Stdlib.List.Car.Tests
import qualified Stdlib.List.Cdr.Tests
import qualified Stdlib.List.Cons.Tests
import qualified Stdlib.List.Exists.Tests
import qualified Stdlib.List.Forall.Tests
import qualified Stdlib.List.Length.Tests
import qualified Stdlib.List.List.Tests
import qualified Stdlib.List.Null.Tests
import qualified Stdlib.List.Reverse.Tests
import qualified Stdlib.Number.Negative.Tests
import qualified Stdlib.Number.Positive.Tests
import qualified Stdlib.Number.Zero.Tests
import qualified Stdlib.String.Length.Tests
import qualified Stdlib.String.StringToList.Tests

main :: IO ()
main = defaultMain $ testGroup "Stdlib" [
    Stdlib.Boolean.And.Tests.tests,
    Stdlib.Boolean.Not.Tests.tests,
    Stdlib.Boolean.Or.Tests.tests,
    Stdlib.Control.Unless.Tests.tests,
    Stdlib.Control.When.Tests.tests,
    Stdlib.Equality.Boolean.Tests.tests,
    Stdlib.Equality.Character.Tests.tests,
    Stdlib.Equality.List.Tests.tests,
    Stdlib.Equality.Number.Tests.tests,
    Stdlib.Equality.Procedure.Tests.tests,
    Stdlib.Equality.String.Tests.tests,
    Stdlib.List.Car.Tests.tests,
    Stdlib.List.Cdr.Tests.tests,
    Stdlib.List.Cons.Tests.tests,
    Stdlib.List.Exists.Tests.tests,
    Stdlib.List.Forall.Tests.tests,
    Stdlib.List.Length.Tests.tests,
    Stdlib.List.List.Tests.tests,
    Stdlib.List.Null.Tests.tests,
    Stdlib.List.Reverse.Tests.tests,
    Stdlib.Number.Negative.Tests.tests,
    Stdlib.Number.Positive.Tests.tests,
    Stdlib.Number.Zero.Tests.tests,
    Stdlib.String.Length.Tests.tests,
    Stdlib.String.StringToList.Tests.tests
 ]
