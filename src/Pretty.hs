{-# LANGUAGE OverloadedStrings #-}

module Pretty where

import LispVal(LispVal(..))

import Data.Text.Lazy(fromStrict)

instance Pretty LispVal where
  pretty expr = case expr of
    Atom str   -> text (fromStrict str)
    Number i   -> pretty i
    String s   -> dquotes (text (fromStrict s))
    Bool True  -> text "#t"
    Bool False -> text "#f"
    List ls    -> encloseSep lparen rparen space (fmap pretty ls)
