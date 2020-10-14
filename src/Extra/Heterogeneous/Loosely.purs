module Extra.Heterogeneous.Loosely
  ( LooselyMapping(..)
  ) where

import Prelude
import Heterogeneous.Mapping (class Mapping, mapping)

-- | Allows mapping to skip non-compatible types.
-- | Restricts mapping type to be a type constructor on input-output types.
-- | WARN: it doesn't work when applied to typeclass-constrained functions.
-- TODO: check if it's possible to go around typeclass-constraint limitation or at least enforce a compiler error when you try to do so
newtype LooselyMapping f
  = LooselyMapping f

instance looselyMappingN :: Mapping (m f g) (f a) (g b) => Mapping (LooselyMapping (m f g)) (f a) (g b) where
  mapping (LooselyMapping m) = mapping m
else instance looselyMapping :: Mapping (f a b) a b => Mapping (LooselyMapping (f a b)) a b where
  mapping (LooselyMapping f) = mapping f
else instance looselyMappingSkip :: Mapping (LooselyMapping f) a a where
  mapping _ = identity
