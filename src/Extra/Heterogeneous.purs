module Extra.Heterogeneous
  ( MappingWithIndex(..)
  , mkRecursiveMappingWithIndexAggregated
  ) where

import Data.List (List(..))
import Data.Symbol (class IsSymbol, SProxy, reflectSymbol)
import Extra.Data.Lens.Indexed (ReversedSegments(..), addSegment)
import Extra.Heterogeneous.Recursive (RecursiveMappingWithIndex(..))
import Heterogeneous.Mapping (class Mapping, class MappingWithIndex, mapping)

-- TODO: Rename `MappingWithIndex` ? It doesn't conflict with `MappingWithIndex` typeclass, but still might be confusing.
-- | There is no default instance of `MappingWithIndex` typeclass for
-- | functions like `i -> a -> b`, so this is a convenient wrapper to
-- | allow such functions to be applied to indexable structures.
newtype MappingWithIndex f
  = MappingWithIndex (forall prop. IsSymbol prop => SProxy prop -> f)

instance mappingWithIndex ::
  ( IsSymbol prop
  , Mapping f a b
  ) =>
  MappingWithIndex
    (MappingWithIndex f)
    (SProxy prop)
    a
    b where
  mappingWithIndex (MappingWithIndex f) prop = mapping (f prop)

-- TODO: it probably makes more sense to have this function at the `Extra.Heterogeneous.Recursive`, but it has dependency on
-- `MappingWithIndex` newtype, so we also need to extract that as separate module as well.
-- | `RecursiveMappingWithIndex` is not particularly convenient for simple use-case. So here is a helper
-- | constructor - if you have a way of mapping a single value (together with it's index path within the structure)
-- | then it can give you a recursive mapping that applies your single-value mapping to the entire structure recursively.
mkRecursiveMappingWithIndexAggregated :: forall f. (ReversedSegments -> f) -> RecursiveMappingWithIndex (MappingWithIndex f)
mkRecursiveMappingWithIndexAggregated mkF =
  RecursiveMappingWithIndex
    { mkF: mkF' (ReversedSegments Nil)
    , f: mkActualF (ReversedSegments Nil)
    }
  where
  mkF' :: forall prop. IsSymbol prop => ReversedSegments -> SProxy prop -> RecursiveMappingWithIndex (MappingWithIndex f)
  mkF' segments prop =
    RecursiveMappingWithIndex
      { mkF: mkF' segments'
      , f: mkActualF segments'
      }
    where
    segment = reflectSymbol prop

    segments' = addSegment segment segments

  mkActualF :: ReversedSegments -> MappingWithIndex f
  mkActualF segments = MappingWithIndex f'
    where
    f' :: forall prop. IsSymbol prop => SProxy prop -> f
    f' prop = mkF segments'
      where
      segment = reflectSymbol prop

      segments' = addSegment segment segments
