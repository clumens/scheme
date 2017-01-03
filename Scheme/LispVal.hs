{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Scheme.LispVal(EnvCtx,
                      Eval(..),
                      LispVal(..),
                      IFunc(..),
                      showVal,
                      typeOf,
                      unwordsList)
 where

import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.State(MonadState, StateT)
import qualified Data.Map as Map
import qualified Data.Text as T
import           Data.Typeable(Typeable)

type EnvCtx = Map.Map T.Text LispVal

newtype Eval a = Eval { unEval :: StateT EnvCtx IO a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadState EnvCtx)

data LispVal = Atom T.Text
             | Bool Bool
             | Character Char
             | Func [T.Text] IFunc
             | Lambda IFunc EnvCtx
             | List [LispVal]
             | Nil
             | Number Integer
             | PrimitiveFunc IFunc
             | String T.Text
 deriving (Show, Typeable)

data IFunc = IFunc { func :: [LispVal] -> Eval LispVal }
 deriving (Typeable)

instance Show IFunc where
    show (IFunc _) = "(function)"

showVal :: LispVal -> T.Text
showVal val = case val of
    Atom atom       -> atom
    Bool True       -> "#t"
    Bool False      -> "#f"
    Character ch    -> T.singleton ch
    Func _ _        -> "(user function)"
    Lambda _ _      -> "(lambda function)"
    List contents   -> T.concat ["(", unwordsList contents, ")"]
    Nil             -> "Nil"
    Number num      -> T.pack $ show num
    PrimitiveFunc _ -> "(internal function)"
    String txt      -> T.concat [ "\"" , txt, "\""]

typeOf :: LispVal -> T.Text
typeOf val = case val of
    Atom _      -> "Atom"
    Bool _      -> "Bool"
    Character _ -> "Character"
    List _      -> "List"
    Nil         -> "Nil"
    Number _    -> "Number"
    String _    -> "String"
    _           -> "Function"

unwordsList :: [LispVal] -> T.Text
unwordsList list = T.unwords $ showVal <$> list
