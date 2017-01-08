{-# LANGUAGE OverloadedStrings #-}

module Stdlib.Quotation.Quote.Tests(tests)
 where

import qualified Data.Text as T
import           Test.Tasty(TestTree, testGroup)

import Stdlib.Run(mkTests)

testData :: [(FilePath, T.Text)]
testData = [
    ("tests/Stdlib/Quotation/Quote/quote-01.scm", "a"),
    ("tests/Stdlib/Quotation/Quote/quote-02.scm", "(a b c)"),
    ("tests/Stdlib/Quotation/Quote/quote-03.scm", "(+ 1 2)"),
    ("tests/Stdlib/Quotation/Quote/quote-04.scm", "\"abcd\""),
    ("tests/Stdlib/Quotation/Quote/quote-05.scm", "12345"),
    ("tests/Stdlib/Quotation/Quote/quote-06.scm", "blah"),
    ("tests/Stdlib/Quotation/Quote/quote-07.scm", "(+ 1 2 3)"),
    ("tests/Stdlib/Quotation/Quote/quote-08.scm", "(quote a)"),
    ("tests/Stdlib/Quotation/Quote/quote-09.scm", "(quote x)")
 ]

tests :: TestTree
tests = testGroup "quote" $ mkTests testData
