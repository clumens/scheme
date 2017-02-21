module Main where

import Test.Tasty(defaultMain, testGroup)

import qualified Stdlib.Boolean.And.Tests
import qualified Stdlib.Boolean.Not.Tests
import qualified Stdlib.Boolean.Or.Tests
import qualified Stdlib.Control.Unless.Tests
import qualified Stdlib.Control.When.Tests
import qualified Stdlib.Equality.Boolean.Tests
import qualified Stdlib.Equality.Character.Tests
import qualified Stdlib.Equality.Condition.Tests
import qualified Stdlib.Equality.List.Tests
import qualified Stdlib.Equality.Number.Tests
import qualified Stdlib.Equality.Procedure.Tests
import qualified Stdlib.Equality.String.Tests
import qualified Stdlib.List.Append.Tests
import qualified Stdlib.List.Car.Tests
import qualified Stdlib.List.Cdr.Tests
import qualified Stdlib.List.Cons.Tests
import qualified Stdlib.List.Exists.Tests
import qualified Stdlib.List.Forall.Tests
import qualified Stdlib.List.Length.Tests
import qualified Stdlib.List.List.Tests
import qualified Stdlib.List.ListToString.Tests
import qualified Stdlib.List.ListRef.Tests
import qualified Stdlib.List.ListTail.Tests
import qualified Stdlib.List.Memp.Tests
import qualified Stdlib.List.Memq.Tests
import qualified Stdlib.List.Null.Tests
import qualified Stdlib.List.Remp.Tests
import qualified Stdlib.List.Remq.Tests
import qualified Stdlib.List.Reverse.Tests
import qualified Stdlib.Math.Abs.Tests
import qualified Stdlib.Math.Dec.Tests
import qualified Stdlib.Math.Div.Tests
import qualified Stdlib.Math.Inc.Tests
import qualified Stdlib.Math.Max.Tests
import qualified Stdlib.Math.Min.Tests
import qualified Stdlib.Math.Mod.Tests
import qualified Stdlib.Number.Even.Tests
import qualified Stdlib.Number.Negative.Tests
import qualified Stdlib.Number.Odd.Tests
import qualified Stdlib.Number.Positive.Tests
import qualified Stdlib.Number.Zero.Tests
import qualified Stdlib.Quotation.Quote.Tests
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
    Stdlib.Equality.Condition.Tests.tests,
    Stdlib.Equality.List.Tests.tests,
    Stdlib.Equality.Number.Tests.tests,
    Stdlib.Equality.Procedure.Tests.tests,
    Stdlib.Equality.String.Tests.tests,
    Stdlib.List.Append.Tests.tests,
    Stdlib.List.Car.Tests.tests,
    Stdlib.List.Cdr.Tests.tests,
    Stdlib.List.Cons.Tests.tests,
    Stdlib.List.Exists.Tests.tests,
    Stdlib.List.Forall.Tests.tests,
    Stdlib.List.Length.Tests.tests,
    Stdlib.List.List.Tests.tests,
    Stdlib.List.ListRef.Tests.tests,
    Stdlib.List.ListTail.Tests.tests,
    Stdlib.List.ListToString.Tests.tests,
    Stdlib.List.Memp.Tests.tests,
    Stdlib.List.Memq.Tests.tests,
    Stdlib.List.Null.Tests.tests,
    Stdlib.List.Remp.Tests.tests,
    Stdlib.List.Remq.Tests.tests,
    Stdlib.List.Reverse.Tests.tests,
    Stdlib.Math.Abs.Tests.tests,
    Stdlib.Math.Dec.Tests.tests,
    Stdlib.Math.Div.Tests.tests,
    Stdlib.Math.Inc.Tests.tests,
    Stdlib.Math.Max.Tests.tests,
    Stdlib.Math.Min.Tests.tests,
    Stdlib.Math.Mod.Tests.tests,
    Stdlib.Number.Even.Tests.tests,
    Stdlib.Number.Negative.Tests.tests,
    Stdlib.Number.Odd.Tests.tests,
    Stdlib.Number.Positive.Tests.tests,
    Stdlib.Number.Zero.Tests.tests,
    Stdlib.Quotation.Quote.Tests.tests,
    Stdlib.String.Length.Tests.tests,
    Stdlib.String.StringToList.Tests.tests
 ]
