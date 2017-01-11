module Scheme.Types(SchemeTy(..))
 where

import qualified Data.Text as T

-- This represents new types that can be defined by the user.  It being scheme,
-- there's not a lot of those.  For now, it's just new conditions.  Later, this is
-- where records could be defined.
data SchemeTy = Condition (Maybe T.Text)
 deriving(Eq, Show)
