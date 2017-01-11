{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Scheme.LispVal(Eval(..),
                      LispVal(..),
                      IFunc(..),
                      SchemeSt(..),
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
data SchemeSt = SchemeSt { stBindings :: Environment LispVal,
                           stTypes :: Environment SchemeTy }
 deriving(Eq, Show)

mkEmptyState :: SchemeSt
mkEmptyState = SchemeSt { stBindings=mkEnvironment [],
                          stTypes=mkEnvironment [] }

newtype Eval a = Eval { unEval :: StateT SchemeSt IO a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadState SchemeSt)

data LispVal = Atom T.Text
             | Bool Bool
             | Character Char
             | Error T.Text T.Text
             | Func IFunc (Maybe SchemeSt)
             | List [LispVal]
             | Nil
             | Number Integer
             | String T.Text
 deriving (Eq, Show, Typeable)

newtype IFunc = IFunc { func :: [LispVal] -> Eval LispVal }
 deriving (Typeable)

instance Eq IFunc where
    (IFunc _) == (IFunc _) = False

instance Show IFunc where
    show (IFunc _) = "(function)"

showVal :: LispVal -> T.Text
showVal val = case val of
    Atom atom       -> atom
    Bool True       -> "#t"
    Bool False      -> "#f"
    Character ch    -> T.singleton ch
    Error ty msg    -> T.concat ["Error (", ty, "):\n", msg]
    Func _ _        -> "(function)"
    List contents   -> T.concat ["(", unwordsList contents, ")"]
    Nil             -> "Nil"
    Number num      -> T.pack $ show num
    String txt      -> T.concat [ "\"" , txt, "\""]

typeOf :: LispVal -> T.Text
typeOf val = case val of
    Atom _      -> "Atom"
    Bool _      -> "Bool"
    Character _ -> "Character"
    Error _ _   -> "Error"
    List _      -> "List"
    Nil         -> "Nil"
    Number _    -> "Number"
    String _    -> "String"
    _           -> "Function"

unwordsList :: [LispVal] -> T.Text
unwordsList list = T.unwords $ showVal <$> list
