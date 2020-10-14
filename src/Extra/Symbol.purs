module Extra.Symbol where

import Prim.Boolean (False, True, kind Boolean)
import Prim.Ordering (EQ, kind Ordering)
import Prim.Symbol (class Append, class Compare)

class IsEQ (ordering :: Ordering) (result :: Boolean) | ordering -> result

instance isEQEQ :: IsEQ EQ True
else instance isEQNotEQ :: IsEQ ordering False

class IsEmpty (prop :: Symbol) (result :: Boolean) | prop -> result

instance isEmpty ::
  ( Compare prop "" ordering
  , IsEQ ordering isEmpty
  ) =>
  IsEmpty prop isEmpty

class AppendDotIfNot (basePath :: Symbol) (appendDot :: Boolean) (result :: Symbol) | basePath appendDot -> result, appendDot result -> basePath

instance appendDotIfNotFalse ::
  Append basePath "." result =>
  AppendDotIfNot basePath False result

instance appendDotIfNotTrue :: AppendDotIfNot basePath True basePath

-- | Type-level constructing of index path string.
-- | Examples
-- | ```
-- | -- these are valid instances
-- | AppendPath "" "a" "a" =>
-- | AppendPath "a" "b" "a.b" =>
-- | AppendPath "a.b" "c" "a.b.c" =>
-- | ```
class AppendPath (basePath :: Symbol) (prop :: Symbol) (result :: Symbol) | basePath prop -> result, prop result -> basePath, basePath result -> prop

instance appendPath ::
  ( IsEmpty basePath isEmpty
  , AppendDotIfNot basePath isEmpty basePath'
  , Append basePath' prop result
  ) =>
  AppendPath basePath prop result
