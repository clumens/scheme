module Scheme.Environment(Environment(..),
                          addToEnvironment,
                          environmentLookup,
                          environmentWords,
                          mkEnvironment)
 where

import qualified Data.Map as Map
import qualified Data.Text as T

data Environment a = Environment (Map.Map T.Text a)
 deriving(Eq, Show)

addToEnvironment :: [(T.Text, a)] -> Environment a -> Environment a
addToEnvironment lst (Environment env) = Environment $ Map.fromList lst `Map.union` env

environmentLookup :: T.Text -> Environment a -> Maybe a
environmentLookup key (Environment env) = Map.lookup key env

environmentWords :: Environment a -> [T.Text]
environmentWords (Environment env) = Map.keys env

mkEnvironment :: [(T.Text, a)] -> Environment a
mkEnvironment lst = Environment $ Map.fromList lst
