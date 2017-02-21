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
    ("tests/Stdlib/Equality/Character/character-08.scm", "#t"),
    ("tests/Stdlib/Equality/Character/character-09.scm", "#f"),

    ("tests/Stdlib/Equality/Character/eq-01.scm", "#t"),
    ("tests/Stdlib/Equality/Character/eq-02.scm", "#f"),
    ("tests/Stdlib/Equality/Character/eq-03.scm", "Error (type-error):\n\tExpected: Char\n\tGot:      Number\n\tIn value: 12"),
    ("tests/Stdlib/Equality/Character/eq-04.scm", "#t"),
    ("tests/Stdlib/Equality/Character/eq-05.scm", "#f"),

    ("tests/Stdlib/Equality/Character/lt-01.scm", "#f"),
    ("tests/Stdlib/Equality/Character/lt-02.scm", "#f"),
    ("tests/Stdlib/Equality/Character/lt-03.scm", "#t"),
    ("tests/Stdlib/Equality/Character/lt-04.scm", "Error (type-error):\n\tExpected: Char\n\tGot:      Atom\n\tIn value: blah"),
    ("tests/Stdlib/Equality/Character/lt-05.scm", "#f"),
    ("tests/Stdlib/Equality/Character/lt-06.scm", "#t"),

    ("tests/Stdlib/Equality/Character/lteq-01.scm", "#t"),
    ("tests/Stdlib/Equality/Character/lteq-02.scm", "#f"),
    ("tests/Stdlib/Equality/Character/lteq-03.scm", "#t"),
    ("tests/Stdlib/Equality/Character/lteq-04.scm", "Error (type-error):\n\tExpected: Char\n\tGot:      Atom\n\tIn value: blah"),
    ("tests/Stdlib/Equality/Character/lteq-05.scm", "#t"),
    ("tests/Stdlib/Equality/Character/lteq-06.scm", "#t"),

    ("tests/Stdlib/Equality/Character/gt-01.scm", "#f"),
    ("tests/Stdlib/Equality/Character/gt-02.scm", "#f"),
    ("tests/Stdlib/Equality/Character/gt-03.scm", "#t"),
    ("tests/Stdlib/Equality/Character/gt-04.scm", "Error (type-error):\n\tExpected: Char\n\tGot:      List\n\tIn value: ()"),
    ("tests/Stdlib/Equality/Character/gt-05.scm", "#f"),
    ("tests/Stdlib/Equality/Character/gt-06.scm", "#t"),

    ("tests/Stdlib/Equality/Character/gteq-01.scm", "#t"),
    ("tests/Stdlib/Equality/Character/gteq-02.scm", "#f"),
    ("tests/Stdlib/Equality/Character/gteq-03.scm", "#t"),
    ("tests/Stdlib/Equality/Character/gteq-04.scm", "Error (type-error):\n\tExpected: Char\n\tGot:      List\n\tIn value: ()"),
    ("tests/Stdlib/Equality/Character/gteq-05.scm", "#t"),
    ("tests/Stdlib/Equality/Character/gteq-06.scm", "#t")
 ]

tests :: TestTree
tests = testGroup "character" $ mkTests testData
