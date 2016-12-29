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
             | List [LispVal]
             | Number Integer
             | String T.Text
             | Character Char
             | PrimitiveFunc IFunc
             | Func [T.Text] IFunc
             | Lambda IFunc EnvCtx
             | Nil
             | Bool Bool
 deriving (Show, Typeable)

data IFunc = IFunc { func :: [LispVal] -> Eval LispVal }
 deriving (Typeable)

instance Show IFunc where
    show (IFunc _) = "(function)"

showVal :: LispVal -> T.Text
showVal val = case val of
    Atom atom       -> atom
    String txt      -> T.concat [ "\"" , txt, "\""]
    Character ch    -> T.singleton ch
    Number num      -> T.pack $ show num
    Bool True       -> "#t"
    Bool False      -> "#f"
    Nil             -> "Nil"
    List contents   -> T.concat ["(", unwordsList contents, ")"]
    PrimitiveFunc _ -> "(internal function)"
    Func _ _        -> "(user function)"
    Lambda _ _      -> "(lambda function)"

typeOf :: LispVal -> T.Text
typeOf val = case val of
    Atom _      -> "Atom"
    String _    -> "String"
    Character _ -> "Character"
    Number _    -> "Number"
    Bool _      -> "Bool"
    Nil         -> "Nil"
    List _      -> "List"
    _           -> "Function"

unwordsList :: [LispVal] -> T.Text
unwordsList list = T.unwords $ showVal <$> list
