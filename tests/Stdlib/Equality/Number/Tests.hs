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
    ("tests/Stdlib/Equality/Number/number-07.scm", "#t"),
    ("tests/Stdlib/Equality/Number/number-08.scm", "#f"),
    ("tests/Stdlib/Equality/Number/number-09.scm", "#t"),
    ("tests/Stdlib/Equality/Number/number-10.scm", "#t"),
    ("tests/Stdlib/Equality/Number/number-11.scm", "#t"),

    ("tests/Stdlib/Equality/Number/integer-01.scm", "#f"),
    ("tests/Stdlib/Equality/Number/integer-02.scm", "#f"),
    ("tests/Stdlib/Equality/Number/integer-03.scm", "#f"),
    ("tests/Stdlib/Equality/Number/integer-04.scm", "#t"),
    ("tests/Stdlib/Equality/Number/integer-05.scm", "#f"),
    ("tests/Stdlib/Equality/Number/integer-06.scm", "#f"),
    ("tests/Stdlib/Equality/Number/integer-07.scm", "#t"),
    ("tests/Stdlib/Equality/Number/integer-08.scm", "#f"),
    ("tests/Stdlib/Equality/Number/integer-09.scm", "#f"),
    ("tests/Stdlib/Equality/Number/integer-10.scm", "#f"),
    ("tests/Stdlib/Equality/Number/integer-11.scm", "#f"),

    ("tests/Stdlib/Equality/Number/real-01.scm", "#f"),
    ("tests/Stdlib/Equality/Number/real-02.scm", "#f"),
    ("tests/Stdlib/Equality/Number/real-03.scm", "#f"),
    ("tests/Stdlib/Equality/Number/real-04.scm", "#t"),
    ("tests/Stdlib/Equality/Number/real-05.scm", "#f"),
    ("tests/Stdlib/Equality/Number/real-06.scm", "#f"),
    ("tests/Stdlib/Equality/Number/real-07.scm", "#t"),
    ("tests/Stdlib/Equality/Number/real-08.scm", "#f"),
    ("tests/Stdlib/Equality/Number/real-09.scm", "#t"),
    ("tests/Stdlib/Equality/Number/real-10.scm", "#t"),
    ("tests/Stdlib/Equality/Number/real-11.scm", "#t"),

    ("tests/Stdlib/Equality/Number/eq-01.scm", "#t"),
    ("tests/Stdlib/Equality/Number/eq-02.scm", "#f"),
    ("tests/Stdlib/Equality/Number/eq-03.scm", "Error (type-error):\n\tExpected: Number\n\tGot:      Character\n\tIn value: a"),
    ("tests/Stdlib/Equality/Number/eq-04.scm", "#t"),
    ("tests/Stdlib/Equality/Number/eq-05.scm", "#f"),

    ("tests/Stdlib/Equality/Number/lt-01.scm", "#f"),
    ("tests/Stdlib/Equality/Number/lt-02.scm", "#t"),
    ("tests/Stdlib/Equality/Number/lt-03.scm", "#f"),
    ("tests/Stdlib/Equality/Number/lt-04.scm", "Error (type-error):\n\tExpected: Number\n\tGot:      Atom\n\tIn value: blah"),
    ("tests/Stdlib/Equality/Number/lt-05.scm", "#f"),
    ("tests/Stdlib/Equality/Number/lt-06.scm", "#t"),

    ("tests/Stdlib/Equality/Number/lteq-01.scm", "#t"),
    ("tests/Stdlib/Equality/Number/lteq-02.scm", "#t"),
    ("tests/Stdlib/Equality/Number/lteq-03.scm", "#f"),
    ("tests/Stdlib/Equality/Number/lteq-04.scm", "Error (type-error):\n\tExpected: Number\n\tGot:      Atom\n\tIn value: blah"),
    ("tests/Stdlib/Equality/Number/lteq-05.scm", "#t"),
    ("tests/Stdlib/Equality/Number/lteq-06.scm", "#t"),

    ("tests/Stdlib/Equality/Number/gt-01.scm", "#t"),
    ("tests/Stdlib/Equality/Number/gt-02.scm", "#f"),
    ("tests/Stdlib/Equality/Number/gt-03.scm", "Error (type-error):\n\tExpected: Number\n\tGot:      List\n\tIn value: ()"),
    ("tests/Stdlib/Equality/Number/gt-04.scm", "#f"),
    ("tests/Stdlib/Equality/Number/gt-05.scm", "#t"),

    ("tests/Stdlib/Equality/Number/gteq-01.scm", "#t"),
    ("tests/Stdlib/Equality/Number/gteq-02.scm", "#t"),
    ("tests/Stdlib/Equality/Number/gteq-03.scm", "#f"),
    ("tests/Stdlib/Equality/Number/gteq-04.scm", "Error (type-error):\n\tExpected: Number\n\tGot:      List\n\tIn value: ()"),
    ("tests/Stdlib/Equality/Number/gteq-05.scm", "#t"),
    ("tests/Stdlib/Equality/Number/gteq-06.scm", "#t")
 ]

tests :: TestTree
tests = testGroup "number" $ mkTests testData
