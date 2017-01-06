{-# LANGUAGE OverloadedStrings #-}

module Stdlib.Equality.String.Tests(tests)
 where

import qualified Data.Text as T
import           Test.Tasty(TestTree, testGroup)

import Stdlib.Run(mkTests)

testData :: [(FilePath, T.Text)]
testData = [
    ("tests/Stdlib/Equality/String/string-01.scm", "#f"),
    ("tests/Stdlib/Equality/String/string-02.scm", "#f"),
    ("tests/Stdlib/Equality/String/string-03.scm", "#f"),
    ("tests/Stdlib/Equality/String/string-04.scm", "#f"),
    ("tests/Stdlib/Equality/String/string-05.scm", "#f"),
    ("tests/Stdlib/Equality/String/string-06.scm", "#t"),
    ("tests/Stdlib/Equality/String/string-07.scm", "#f"),
    ("tests/Stdlib/Equality/String/string-08.scm", "#f"),

    ("tests/Stdlib/Equality/String/eq-01.scm", "#t"),
    ("tests/Stdlib/Equality/String/eq-02.scm", "#t"),
    ("tests/Stdlib/Equality/String/eq-03.scm", "#f"),
    ("tests/Stdlib/Equality/String/eq-04.scm", "#f"),
    ("tests/Stdlib/Equality/String/eq-05.scm", "#f"),
    ("tests/Stdlib/Equality/String/eq-06.scm", "#f"),
    ("tests/Stdlib/Equality/String/eq-07.scm", "#t"),
    ("tests/Stdlib/Equality/String/eq-08.scm", "#f"),

    ("tests/Stdlib/Equality/String/lt-01.scm", "#f"),
    ("tests/Stdlib/Equality/String/lt-02.scm", "#f"),
    ("tests/Stdlib/Equality/String/lt-03.scm", "#f"),
    ("tests/Stdlib/Equality/String/lt-04.scm", "#t"),
    ("tests/Stdlib/Equality/String/lt-05.scm", "#f"),
    ("tests/Stdlib/Equality/String/lt-06.scm", "#t"),

    ("tests/Stdlib/Equality/String/lteq-01.scm", "#t"),
    ("tests/Stdlib/Equality/String/lteq-02.scm", "#t"),
    ("tests/Stdlib/Equality/String/lteq-03.scm", "#f"),
    ("tests/Stdlib/Equality/String/lteq-04.scm", "#t"),
    ("tests/Stdlib/Equality/String/lteq-05.scm", "#f"),
    ("tests/Stdlib/Equality/String/lteq-06.scm", "#t"),

    ("tests/Stdlib/Equality/String/gt-01.scm", "#f"),
    ("tests/Stdlib/Equality/String/gt-02.scm", "#f"),
    ("tests/Stdlib/Equality/String/gt-03.scm", "#t"),
    ("tests/Stdlib/Equality/String/gt-04.scm", "#f"),
    ("tests/Stdlib/Equality/String/gt-05.scm", "#t"),
    ("tests/Stdlib/Equality/String/gt-06.scm", "#f"),

    ("tests/Stdlib/Equality/String/gteq-01.scm", "#t"),
    ("tests/Stdlib/Equality/String/gteq-02.scm", "#t"),
    ("tests/Stdlib/Equality/String/gteq-03.scm", "#t"),
    ("tests/Stdlib/Equality/String/gteq-04.scm", "#f"),
    ("tests/Stdlib/Equality/String/gteq-05.scm", "#t"),
    ("tests/Stdlib/Equality/String/gteq-06.scm", "#f")
 ]

tests :: TestTree
tests = testGroup "string" $ mkTests testData
