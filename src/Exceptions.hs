{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Exceptions(LispException(..),
                  showError)
 where

import LispVal(LispVal(..), unwordsList, showVal)

import           Control.Exception(Exception)
import qualified Data.Text as T
import           Data.Typeable(Typeable)

data LispException = NumArgs Integer [LispVal]
                   | LengthOfList T.Text Int
                   | ExpectedList T.Text
                   | TypeMismatch T.Text LispVal
                   | BadSpecialForm T.Text
                   | NotFunction LispVal
                   | UnboundVar T.Text
                   | Default LispVal
                   | PError String
                   | IOError T.Text
 deriving (Typeable)

instance Exception LispException

instance Show LispException where
  show = T.unpack . showError

showError :: LispException -> T.Text
showError err = case err of
    IOError txt            -> T.concat ["Error reading file: ", txt]
    NumArgs int args       -> T.concat ["Error Number Arguments, expected ", T.pack $ show int, " recieved args: ", unwordsList args]
    LengthOfList txt int   -> T.concat ["Error Length of List in ", txt, " length: ", T.pack $ show int]
    ExpectedList txt       -> T.concat ["Error Expected List in funciton ", txt]
    TypeMismatch txt val   -> T.concat ["Error Type Mismatch: ", txt, showVal val]
    BadSpecialForm txt     -> T.concat ["Error Bad Special Form: ", txt]
    NotFunction val        -> T.concat ["Error Not a Function: ", showVal val]
    UnboundVar txt         -> T.concat ["Error Unbound Variable: ", txt]
    PError str             -> T.concat ["Parser Error, expression cannot evaluate: ",T.pack str]
    Default val            -> T.concat ["Error, Danger Will Robinson! Evaluation could not proceed!  ", showVal val]
