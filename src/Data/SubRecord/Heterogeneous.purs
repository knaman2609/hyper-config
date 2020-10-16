module Data.SubRecord.Heterogeneous where

import Prelude
import Control.Alt ((<|>))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.SubRecord (SubRecord, mkSubRecord, toSubRecord)
import Data.SubRecord.Heterogeneous.Nullable (class HFromNullable, hfromNullable)
import Data.Tuple (Tuple(..))
import Extra.Debug (notImplemented)
import Extra.Heterogeneous.Zip (class HZipRight, hzipRight)
import Heterogeneous.Mapping (class HMap, class Mapping, hmap, mapping)
import HyperWidgetConfig.Reference (Reference(..))
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row (class Union)
import Prim.Row as Row
import Prim.RowList (class RowToList, Nil, kind RowList)
import Prim.RowList as RL
import Prim.TypeError (class Fail, Above, Quote, Text)
import Type.RowList (class ListToRow)
import Unsafe.Coerce (unsafeCoerce)

class HFillFromSmallerRecursive env r where
  -- | Recursively goes through `r` and replaces missing properties with properties found in the `env`.
  hfillFromSmallerRecursive :: env -> r -> r

instance hfillFromSmallerRecursiveRS ::
  ( HZipRight (Record env) (SubRecord r) (Record zipped)
  , HMap
      (FillFromSmallerRecursiveMapping)
      (Record zipped)
      (Record nullable)
  , HFromNullable (Record nullable) (SubRecord r)
  ) =>
  HFillFromSmallerRecursive (Record env) (SubRecord r) where
  hfillFromSmallerRecursive env r = result
    where
    zipped :: Record zipped
    zipped = hzipRight env r

    nullable :: Record nullable
    nullable = hmap FillFromSmallerRecursiveMapping zipped

    result :: SubRecord r
    result = hfromNullable nullable

instance hfillFromSmallerRecursiveSS ::
  ( HZipRight (SubRecord env) (SubRecord r) (Record zipped)
  , HMap
      (FillFromSmallerRecursiveMapping)
      (Record zipped)
      (Record nullable)
  , HFromNullable (Record nullable) (SubRecord r)
  ) =>
  HFillFromSmallerRecursive (SubRecord env) (SubRecord r) where
  hfillFromSmallerRecursive env r = result
    where
    zipped :: Record zipped
    zipped = hzipRight env r

    nullable :: Record nullable
    nullable = hmap FillFromSmallerRecursiveMapping zipped

    result :: SubRecord r
    result = hfromNullable nullable

instance hfillFromSmallerRecursiveRR ::
  ( HZipRight (Record env) (Record r) (Record zipped)
  , HMap
      (FillFromSmallerRecursiveMapping)
      (Record zipped)
      (Record r)
  ) =>
  HFillFromSmallerRecursive (Record env) (Record r) where
  hfillFromSmallerRecursive env r = result
    where
    zipped :: Record zipped
    zipped = hzipRight env r

    result :: Record r
    result = hmap FillFromSmallerRecursiveMapping zipped

data FillFromSmallerRecursiveMapping
  = FillFromSmallerRecursiveMapping

instance fillFromSmallerRecursiveMappingAandNS ::
  Mapping
      FillFromSmallerRecursiveMapping
      (Tuple a (SubRecord s))
      (SubRecord s) =>
  Mapping
    FillFromSmallerRecursiveMapping
    (Tuple a (Nullable (SubRecord s)))
    (Nullable (SubRecord s)) where
  mapping f (Tuple env ns) =
    Nullable.notNull case Nullable.toMaybe ns of
      -- this is questionable... it is to handle case
      -- when there is null subrecord in r, but
      -- we have a record in l, so we can still grab some
      -- props from l but we don't hit `Tuple a (Nullable a)` case,
      -- so need a specialized one. We don't need analogous
      -- case for Records, because we wouldn't be able to fully restore
      -- Record from smaller env anyway.
      Nothing -> mapping f (Tuple env (mkSubRecord {} :: SubRecord s))
      Just s -> mapping f (Tuple env s)
else instance fillFromSmallerRecursiveMappingNAandNA ::
  Mapping
      FillFromSmallerRecursiveMapping
      (Tuple a a)
      a =>
  Mapping
    FillFromSmallerRecursiveMapping
    (Tuple (Nullable a) (Nullable a))
    (Nullable a) where
  mapping f (Tuple nenv na) = case Nullable.toMaybe nenv of
    Nothing -> na
    Just env -> case Nullable.toMaybe na of
      Nothing -> nenv
      Just a -> Nullable.notNull $ mapping f (Tuple env a)
else instance fillFromSmallerRecursiveMappingNAandNB ::
  Mapping
      FillFromSmallerRecursiveMapping
      (Tuple a b)
      b =>
  Mapping
    FillFromSmallerRecursiveMapping
    (Tuple (Nullable a) (Nullable b))
    (Nullable b) where
  mapping f (Tuple na nb) = case Nullable.toMaybe na of
    Nothing -> nb
    Just a -> case Nullable.toMaybe nb of
      Nothing -> Nullable.null
      Just b -> Nullable.notNull $ mapping f (Tuple a b)
else instance fillFromSmallerRecursiveMappingNAandB ::
  Mapping
      FillFromSmallerRecursiveMapping
      (Tuple env a)
      a =>
  Mapping
    FillFromSmallerRecursiveMapping
    (Tuple (Nullable env) a)
    a where
  mapping f (Tuple nenv a) = case Nullable.toMaybe nenv of
    Nothing -> a
    Just env -> mapping f (Tuple env a)
else instance fillFromSmallerRecursiveMappingAandNA ::
  Mapping
      FillFromSmallerRecursiveMapping
      (Tuple a a)
      a =>
  Mapping
    FillFromSmallerRecursiveMapping
    (Tuple a (Nullable a))
    (Nullable a) where
  mapping f (Tuple env na) = case Nullable.toMaybe na of
    Nothing -> Nullable.notNull env
    Just a -> Nullable.notNull $ mapping f (Tuple env a)
else instance fillFromSmallerRecursiveMappingAandNB ::
  Mapping
      FillFromSmallerRecursiveMapping
      (Tuple env a)
      a =>
  Mapping
    FillFromSmallerRecursiveMapping
    (Tuple env (Nullable a))
    (Nullable a) where
  mapping f (Tuple env na) = case Nullable.toMaybe na of
    Nothing -> na
    Just a -> Nullable.notNull $ mapping f (Tuple env a)
else instance fillFromSmallerRecursiveMappingRecord ::
  HFillFromSmallerRecursive env (Record a) =>
  Mapping
    FillFromSmallerRecursiveMapping
    (Tuple env (Record a))
    (Record a) where
  mapping _ (Tuple env a) = hfillFromSmallerRecursive env a -- still need to traverse records just in case there are inner SubRecords
else instance fillFromSmallerRecursiveMappingSubRecord ::
  HFillFromSmallerRecursive env (SubRecord a) =>
  Mapping
    FillFromSmallerRecursiveMapping
    (Tuple env (SubRecord a))
    (SubRecord a) where
  mapping _ (Tuple env a) = hfillFromSmallerRecursive env a
else instance fillFromSmallerRecursiveMapping ::
  Mapping
    FillFromSmallerRecursiveMapping
    (Tuple env a)
    a where
  mapping _ (Tuple _ a) = a

class HFillFromBiggerRecursive env from to | env from -> to where
  -- | Recursively goes through `from` and replaces missing properties with properties found in the `env`.
  -- | It is to just fill properties missing in `from` (for SubRecords). It still traverses Records as well just
  -- | in case they have inner SubRecords. So the main transformation is that it recursively changes SubRecords to Records
  -- | IF there is a Record in `env` at this place.
  -- | Note: It only grabs what's "necessary" from `env`, it is NOT an override of `env` by `from`.
  hfillFromBiggerRecursive :: env -> from -> to

instance hfillFromBiggerRecursiveRandSR ::
  ( HZipRight (Record env) (SubRecord from) (Record zipped)
  , HMap
      (FillFromBiggerRecursiveMapping)
      (Record zipped)
      (Record to)
  ) =>
  HFillFromBiggerRecursive (Record env) (SubRecord from) (Record to) where
  hfillFromBiggerRecursive env from = to
    where
    zipped :: Record zipped
    zipped = hzipRight env from

    to :: Record to
    to = hmap FillFromBiggerRecursiveMapping zipped

instance hfillFromBiggerRecursiveSRandSR ::
  ( HZipRight (SubRecord env) (SubRecord from) (Record zipped)
  , HMap
      (FillFromBiggerRecursiveMapping)
      (Record zipped)
      (Record nullable)
  , HFromNullable (Record nullable) (SubRecord to)
  ) =>
  HFillFromBiggerRecursive (SubRecord env) (SubRecord from) (SubRecord to) where
  hfillFromBiggerRecursive env from = to
    where
    zipped :: Record zipped
    zipped = hzipRight env from

    nullable :: Record nullable
    nullable = hmap FillFromBiggerRecursiveMapping zipped

    to :: SubRecord to
    to = hfromNullable nullable

data FillFromBiggerRecursiveMapping
  = FillFromBiggerRecursiveMapping

-- TODO: add tracing prop for better debug messages (similar to `ComponentMap1`)
-- eliminating not supported cases
instance fillFromBiggerRecursiveMappingNRandNR ::
  Fail (Text "Case when both sides are Nullable Records is not supported right now, not clear what should happen.") =>
  Mapping
    FillFromBiggerRecursiveMapping
    (Tuple (Nullable (Record env)) (Nullable (Record from)))
    (to) where
  mapping = unsafeCrashWith "unreachable"
else instance fillFromBiggerRecursiveMappingNRandNS ::
  Fail (Text "Case when `env` is Nullable Record and `from` is Nullable SubRecord is not supported right now, not clear what should happen.") =>
  Mapping
    FillFromBiggerRecursiveMapping
    (Tuple (Nullable (Record env)) (Nullable (SubRecord from)))
    (to) where
  mapping = unsafeCrashWith "unreachable"
-- getting rid of unnecessary recursions before simplifying "equivalent" structures
else instance fillFromBiggerRecursiveMappingNSandNS ::
  Mapping
      FillFromBiggerRecursiveMapping
      (Tuple (SubRecord env) (SubRecord from))
      (SubRecord to) =>
  Mapping
    FillFromBiggerRecursiveMapping
    (Tuple (Nullable (SubRecord env)) (Nullable (SubRecord from)))
    (Nullable (SubRecord to)) where
  mapping f (Tuple nenv nfrom) = case Nullable.toMaybe nenv, Nullable.toMaybe nfrom of
    Nothing, Nothing -> Nullable.null
    Just env, Nothing -> Nullable.notNull $ mapping f (Tuple env (mkSubRecord {} :: SubRecord from))
    Nothing, Just from -> Nullable.notNull $ mapping f (Tuple (mkSubRecord {} :: SubRecord env) from)
    Just env, Just from -> Nullable.notNull $ mapping f (Tuple env from)
-- simplify recursive structures
--
-- treat Nullable Records as Nullable SubRecords
-- we can do that since we denied case "NR and NR".
else instance fillFromBiggerRecursiveMappingNRtoNS ::
  Mapping
      FillFromBiggerRecursiveMapping
      (Tuple env (Nullable (SubRecord from)))
      to =>
  Mapping
    FillFromBiggerRecursiveMapping
    (Tuple env (Nullable (Record from)))
    to where
  mapping f (Tuple env nfrom) =
    mapping f
      ( Tuple env
          $ case Nullable.toMaybe nfrom of
              Nothing -> Nullable.null :: Nullable (SubRecord from)
              Just from -> Nullable.notNull $ toSubRecord from
      )
-- treat Records as SubRecords
-- we can do that since our env must be stricter bigger, so we will restore subrecord to record anyway.
else instance fillFromBiggerRecursiveMappingRtoS ::
  Mapping
      FillFromBiggerRecursiveMapping
      (Tuple env (SubRecord from))
      to =>
  Mapping
    FillFromBiggerRecursiveMapping
    (Tuple env (Record from))
    to where
  mapping f (Tuple env from) = mapping f (Tuple env (toSubRecord from))
-- treat Nullable SubRecords as SubRecords
else instance fillFromBiggerRecursiveMappingNStoS ::
  Mapping
      FillFromBiggerRecursiveMapping
      (Tuple env (SubRecord from))
      to =>
  Mapping
    FillFromBiggerRecursiveMapping
    (Tuple env (Nullable (SubRecord from)))
    to where
  mapping f (Tuple env nfrom) =
    mapping f
      ( Tuple env case Nullable.toMaybe nfrom of
          Nothing -> mkSubRecord {} :: SubRecord from
          Just from -> from
      )
-- actual recursion
else instance fillFromBiggerRecursiveMappingEtoS ::
  HFillFromBiggerRecursive env (SubRecord from) to =>
  Mapping
    FillFromBiggerRecursiveMapping
    (Tuple env (SubRecord from))
    (to) where
  mapping _ (Tuple env from) = hfillFromBiggerRecursive env from
-- handling actual values
else instance fillFromBiggerRecursiveMappingNAandNA ::
  Mapping
    FillFromBiggerRecursiveMapping
    (Tuple (Nullable a) (Nullable a))
    (Nullable a) where
  mapping _ (Tuple nenv nfrom) = Nullable.toNullable (Nullable.toMaybe nfrom <|> Nullable.toMaybe nenv)
else instance fillFromBiggerRecursiveMappingNAandNB ::
  Fail
      ( Above
          ( Above (Text "Case when `env` is Nullable value and `from` Nullable value of different type is not supported - `env` must be stricter \"bigger\" than `from`.")
              (Quote env)
          )
          (Quote from)
      ) =>
  Mapping
    FillFromBiggerRecursiveMapping
    (Tuple (Nullable env) (Nullable from))
    (to) where
  mapping = unsafeCrashWith "unreachable"
else instance fillFromBiggerRecursiveMappingNAandB ::
  Fail (Text "Case when `env` is Nullable and `from` is actual value is not supported - `env` must be \"bigger\" than `from`.") =>
  Mapping
    FillFromBiggerRecursiveMapping
    (Tuple (Nullable env) from)
    (to) where
  mapping = unsafeCrashWith "unreachable"
else instance fillFromBiggerRecursiveMappingAandNA ::
  Mapping
    FillFromBiggerRecursiveMapping
    (Tuple a (Nullable a))
    (a) where
  mapping _ (Tuple env nfrom) = fromMaybe env $ Nullable.toMaybe nfrom
else instance fillFromBiggerRecursiveMappingAandA ::
  Mapping
    FillFromBiggerRecursiveMapping
    (Tuple a a)
    (a) where
  mapping _ (Tuple _ from) = from
else instance fillFromBiggerRecursiveMappingAandB ::
  Fail
      ( Above
          ( Above (Text "Case when `env` and `from` of different types is not supported - `env` must be stricter \"bigger\" than `from`.")
              (Quote env)
          )
          (Quote from)
      ) =>
  Mapping
    FillFromBiggerRecursiveMapping
    (Tuple env from)
    (to) where
  mapping = unsafeCrashWith "unreachable"

mkSubRecordRecursive :: forall from to. MkSubRecordRecursive from to => Record from -> SubRecord to
mkSubRecordRecursive = unsafeCoerce

class MkSubRecordRecursive (from :: # Type) (to :: # Type)

instance mkSubRecordRecursiveInst ::
  ( RowToList from lfrom
  , MkSubRecordRecursiveRowList lfrom to
  , ListToRow lfrom from
  ) =>
  MkSubRecordRecursive from to

class MkSubRecordRecursiveRowList (lfrom :: RowList) (to :: # Type)

instance mkSubRecordRecursiveRowListNil :: MkSubRecordRecursiveRowList Nil to

instance mkSubRecordRecursiveRowListR ::
  ( Row.Cons prop (SubRecord dto) tto to
  , MkSubRecordRecursive dfrom dto
  , MkSubRecordRecursiveRowList tfrom tto
  ) =>
  MkSubRecordRecursiveRowList
    (RL.Cons prop (Record dfrom) tfrom)
    to
else instance mkSubRecordRecursiveRowListCons ::
  ( Row.Cons prop v tto to
  , MkSubRecordRecursiveRowList tfrom tto
  ) =>
  MkSubRecordRecursiveRowList
    (RL.Cons prop v tfrom)
    to
