{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module LispVal where

import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Reader(MonadReader, ReaderT)
import qualified Data.Map as Map
import qualified Data.Text as T
import           Data.Typeable(Typeable)

type EnvCtx = Map.Map T.Text LispVal

newtype Eval a = Eval { unEval :: ReaderT EnvCtx IO a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader EnvCtx)

data LispVal = Atom T.Text
             | List [LispVal]
             | Number Integer
             | String T.Text
             | Fun IFunc
             | Lambda IFunc EnvCtx
             | Nil
             | Bool Bool
 deriving (Typeable)

instance Show LispVal where
  show = T.unpack . showVal

data IFunc = IFunc { fn :: [LispVal] -> Eval LispVal }
 deriving (Typeable)

showVal :: LispVal -> T.Text
showVal val = case val of
    Atom atom     -> atom
    String txt    -> T.concat [ "\"" , txt, "\""]
    Number num    -> T.pack $ show num
    Bool True     -> "#t"
    Bool False    -> "#f"
    Nil           -> "Nil"
    List contents -> T.concat ["(", unwordsList contents, ")"]
    Fun _         -> "(internal function)"
    Lambda _ _    -> "(lambda function)"

unwordsList :: [LispVal] -> T.Text
unwordsList list = T.unwords $ showVal <$> list
