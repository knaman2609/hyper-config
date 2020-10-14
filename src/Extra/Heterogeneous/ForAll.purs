module Extra.Heterogeneous.ForAll
  ( ForAllMapping(..)
  , ForAllMappingF(..)
  , ForAllMappingUnF(..)
  , ForAllMappingN(..)
  ) where

import Heterogeneous.Mapping (class Mapping)

-- TODO: check if it's possible to allow arbitrary additional parameters (like typeclass constraint dictionaries) by probably using purescript-reflections
-- | Allows `forall` functions to be applied simultaneously on all types.
-- | Function is restricted to have fixed output type.
newtype ForAllMapping b
  = ForAllMapping (forall a. a -> b)

instance forAllMapping :: Mapping (ForAllMapping b) a b where
  mapping (ForAllMapping f) = f

-- | Allows `forall` functions to be applied simultaneously on all types.
-- | Function's output type is restricted to be constructed from the input type.
newtype ForAllMappingF f
  = ForAllMappingF (forall a. a -> f a)

instance forAllMappingF :: Mapping (ForAllMappingF f) a (f a) where
  mapping (ForAllMappingF f) = f

-- | Allows `forall` functions to be applied simultaneously on all types.
-- | Function's input type is restricted to be constructed from the output type.
newtype ForAllMappingUnF f
  = ForAllMappingUnF (forall a. f a -> a)

instance forAllMappingUnF :: Mapping (ForAllMappingUnF f) (f a) a where
  mapping (ForAllMappingUnF f) = f

-- | Allows `forall` functions to be applied simultaneously on all types.
-- | Function is restricted to be a transformation of the type constructor types.
newtype ForAllMappingN f g
  = ForAllMappingN (forall a. f a -> g a)

instance forAllMappingN :: Mapping (ForAllMappingN f g) (f a) (g a) where
  mapping (ForAllMappingN m) = m
