module Data.SubRecord.Heterogeneous.Nullable where

import Data.Nullable (Nullable)
import Data.SubRecord (SubRecord)
import Extra.Heterogeneous.ForAll (ForAllMappingF, ForAllMappingUnF)
import Heterogeneous.Mapping (class HMap)
import Unsafe.Coerce (unsafeCoerce)

class HToNullable from to | from -> to where
  htoNullable :: from -> to

instance htoNullableSubRecord ::
  HMap (ForAllMappingF Nullable) (Record from) (Record to) =>
  HToNullable (SubRecord from) (Record to) where
  htoNullable = unsafeCoerce -- we can do hmap no problem, but this is faster

class HFromNullable nullable notNullable | nullable -> notNullable where
  hfromNullable :: nullable -> notNullable

instance hfromNullableSubRecord ::
  HMap (ForAllMappingUnF Nullable) (Record nullable) (Record notNullable) =>
  HFromNullable (Record nullable) (SubRecord notNullable) where
  hfromNullable = unsafeCoerce -- we can do hmap no problem, but this is faster
