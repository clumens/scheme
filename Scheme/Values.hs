{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Scheme.Values(Eval(..),
                     IFunc(..),
                     SchemeSt(..),
                     Value(..),
                     mkEmptyState,
                     showVal,
                     typeOf,
                     unwordsList)
 where

import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.State(MonadState, StateT)
import qualified Data.Text as T
import           Data.Typeable(Typeable)

import Scheme.Environment(Environment(..), mkEnvironment)
import Scheme.Types(SchemeTy(..))

-- Program state consists of two mappings: An environment storing bindings and
-- an environment storing new types.
data SchemeSt = SchemeSt { stBindings :: Environment Value,
                           stTypes :: Environment SchemeTy }
 deriving(Eq, Show)

mkEmptyState :: SchemeSt
mkEmptyState = SchemeSt { stBindings=mkEnvironment [],
                          stTypes=mkEnvironment [] }

newtype Eval a = Eval { unEval :: StateT SchemeSt IO a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadState SchemeSt)

data Value = Atom T.Text
           | Bool Bool
           | Character Char
           | Condition T.Text T.Text
           | Float Double
           | Func IFunc (Maybe SchemeSt)
           | List [Value]
           | Nil
           | Number Integer
           | Raised T.Text T.Text
           | String T.Text
 deriving (Eq, Show, Typeable)

newtype IFunc = IFunc { func :: [Value] -> Eval Value }
 deriving (Typeable)

instance Eq IFunc where
    (IFunc _) == (IFunc _) = False

instance Show IFunc where
    show (IFunc _) = "(function)"

showVal :: Value -> T.Text
showVal val = case val of
    Atom atom       -> atom
    Bool True       -> "#t"
    Bool False      -> "#f"
    Character ch    -> T.singleton ch
    Condition ty _  -> T.concat ["(", ty, ")"]
    Float num       -> T.pack $ show num
    Func _ _        -> "(function)"
    List contents   -> T.concat ["(", unwordsList contents, ")"]
    Nil             -> "Nil"
    Number num      -> T.pack $ show num
    Raised ty msg   -> T.concat ["Error (", ty, "):\n", msg]
    String txt      -> T.concat [ "\"" , txt, "\""]

typeOf :: Value -> T.Text
typeOf val = case val of
    Atom _          -> "Atom"
    Bool _          -> "Bool"
    Character _     -> "Character"
    Condition _ _   -> "Condition"
    Float _         -> "Float"
    List _          -> "List"
    Nil             -> "Nil"
    Number _        -> "Number"
    String _        -> "String"
    _               -> "Function"

unwordsList :: [Value] -> T.Text
unwordsList list = T.unwords $ showVal <$> list
