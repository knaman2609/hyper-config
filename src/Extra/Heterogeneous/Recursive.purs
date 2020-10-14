module Extra.Heterogeneous.Recursive
  ( RecursiveMapping(..)
  , RecursiveMappingWithIndex(..)
  , TotalMapping(..)
  ) where

import Prelude
import Data.SubRecord (SubRecord)
import Data.Symbol (class IsSymbol, SProxy)
import Heterogeneous.Mapping (class HMap, class HMapWithIndex, class Mapping, class MappingWithIndex, hmap, hmapWithIndex, mapping, mappingWithIndex)

-- | "Lifts" single-value mapping into being recursively applied to the
-- | Records and SubRecords.
newtype RecursiveMapping f
  = RecursiveMapping f

instance recursiveMappingRecord ::
  HMap (RecursiveMapping f) (Record rin) (Record rout) =>
  Mapping (RecursiveMapping f) (Record rin) (Record rout) where
  mapping = hmap
else instance recursiveMappingSubRecord ::
  HMap (RecursiveMapping f) (SubRecord rin) (SubRecord rout) =>
  Mapping (RecursiveMapping f) (SubRecord rin) (SubRecord rout) where
  mapping = hmap
else instance recursiveMappingValue :: Mapping f a b => Mapping (RecursiveMapping f) a b where
  mapping (RecursiveMapping f) = mapping f

-- | "Lifts" single-value mapping into being recursively applied to the
-- | Records and SubRecords while providing current index prop.
-- | This is a recursive type, somewhat similar to the Mealy machine,
-- | and it provides a way of possibly aggregating prop path.
-- | Check out `Extra.Heterogeneous.mkRecursiveMappingWithIndexAggregated`.
newtype RecursiveMappingWithIndex f
  = RecursiveMappingWithIndex
  { mkF :: forall prop. IsSymbol prop => SProxy prop -> RecursiveMappingWithIndex f
  , f :: f
  }

instance recursiveMappingWithIndexRecord ::
  ( IsSymbol prop
  , HMapWithIndex
      (RecursiveMappingWithIndex f)
      (Record rin)
      (Record rout)
  ) =>
  MappingWithIndex
    (RecursiveMappingWithIndex f)
    (SProxy prop)
    (Record rin)
    (Record rout) where
  mappingWithIndex (RecursiveMappingWithIndex { mkF }) prop = hmapWithIndex (mkF prop)
else instance recursiveMappingWithIndexSubRecord ::
  ( IsSymbol prop
  , HMapWithIndex
      (RecursiveMappingWithIndex f)
      (SubRecord rin)
      (SubRecord rout)
  ) =>
  MappingWithIndex
    (RecursiveMappingWithIndex f)
    (SProxy prop)
    (SubRecord rin)
    (SubRecord rout) where
  mappingWithIndex (RecursiveMappingWithIndex { mkF }) prop = hmapWithIndex (mkF prop)
else instance recursiveMappingWithIndexValue ::
  MappingWithIndex
      f
      (SProxy prop)
      a
      b =>
  MappingWithIndex
    (RecursiveMappingWithIndex f)
    (SProxy prop)
    a
    b where
  mappingWithIndex (RecursiveMappingWithIndex { f }) = mappingWithIndex f

-- | In addition to recursively "dive" into records and subrecords, it also applies the mapping to the dived record/subrecord itself.
-- | (excluding top-level record/subrecord, if mapping over record/subrecord originally).
newtype TotalMapping f
  = TotalMapping f

instance totalMappingRecord ::
  ( HMap (TotalMapping f) (Record rin) (Record rout)
  , Mapping f (Record rout) b
  ) =>
  Mapping (TotalMapping f) (Record rin) b where
  mapping m@(TotalMapping f) = mapping f <<< hmap m
else instance totalMappingSubRecord ::
  ( HMap (TotalMapping f) (SubRecord rin) (SubRecord rout)
  , Mapping f (SubRecord rout) b
  ) =>
  Mapping (TotalMapping f) (SubRecord rin) b where
  mapping m@(TotalMapping f) = mapping f <<< hmap m
else instance totalMappingValue :: Mapping f a b => Mapping (TotalMapping f) a b where
  mapping (TotalMapping f) = mapping f
