-- TODO: Cleanup this file. Think how to deal with circular dependencies, with Builder or Heterogeneous stuff.
-- | Heavily inspired by http://github.com/rubenpieters/purescript-subrecord
module Data.SubRecord
  ( SubRecord
  , toSubRecord
  , mkSubRecord
  , runSubRecord
  , mergeSubRecord
  , unsafeGet
  , get
  , unsafeFromSubRecord
  , class EqSubRecord
  , eqSubRecord
  , class ShowSubRecordFields
  , showSubRecordFields
  -- heterogeneous
  , SRtoR(..)
  , hmapRtoSR
  , hmapSRtoR
  , hmapWithIndexRtoSR
  , hmapWithIndexSRtoR
  , class MapSubRecordWithIndex
  , mapSubRecordWithIndexBuilder
  , class MapSubRecordToRecordWithIndex
  , mapSubRecordToRecordWithIndexBuilder
  , class FoldlSubRecord
  , foldlSubRecordRowList
  -- builder
  , Builder
  , build
  , insert
  , replace
  , modify
  , delete
  , rename
  , merge
  ) where

import Prelude
import Data.Array (cons)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe)
import Data.String (joinWith)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Extra.Data.Function (unsafeApplyWithUndefinedInstance)
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldl, class HFoldlWithIndex, ConstFolding(..), foldingWithIndex)
import Heterogeneous.Mapping (class HMap, class HMapWithIndex, class MappingWithIndex, ConstMapping(..), hmap, hmapWithIndex, mappingWithIndex)
import Prim.Row (class Union)
import Prim.Row as Row
import Prim.RowList (kind RowList)
import Prim.RowList as RL
import Record.Builder as Record.Builder
import Record.Unsafe.Union (unsafeUnion)
import Type.Data.RowList (RLProxy(..))
import Unsafe.Coerce (unsafeCoerce)

-- | Existential type for record subrecords. It is to denote a `Record` with possibly missing fields,
-- | it is isomorphic to `Record` where each property value is wrapped within a `Nullable`. In fact, they even have the same
-- | runtime representation and can be coerced from one to another (check `Data.SubRecord.Heterogeneous.Nullable`).
foreign import data SubRecord :: # Type -> Type

foreign import unsafeGetImpl :: forall p a. Fn2 String (SubRecord p) (Nullable a)

instance showSubRecord :: (RL.RowToList rs ls, ShowSubRecordFields ls rs) => Show (SubRecord rs) where
  show s = case showSubRecordFields (RLProxy :: RLProxy ls) s of
    [] -> "{}"
    fields -> joinWith " " [ "SubRecord (", joinWith ", " fields, ")" ]

instance eqSubRec :: (RL.RowToList row list, EqSubRecord list row) => Eq (SubRecord row) where
  eq = eqSubRecord (RLProxy :: RLProxy list)

instance semigroupSubRecord :: Semigroup (SubRecord p) where
  -- | For common properties `b` takes precedence.
  -- | It's an overriding append - it does not append props, it overrides them.
  append :: SubRecord p -> SubRecord p -> SubRecord p
  append a b = unsafeCoerce $ runSubRecord (runSubRecord unsafeUnion b) a

instance monoidSubRecord :: Monoid (SubRecord p) where
  mempty :: SubRecord p
  mempty = mkSubRecord {}

toSubRecord :: forall p. Record p -> SubRecord p
toSubRecord = unsafeCoerce

unsafeToSubRecord :: forall p1 p2. Record p1 -> SubRecord p2
unsafeToSubRecord = unsafeCoerce

unsafeFromSubRecord :: forall p. SubRecord p -> Record p
unsafeFromSubRecord = unsafeCoerce

mkSubRecord :: forall a a_ p. Union a a_ p => Record a -> SubRecord p
mkSubRecord = unsafeCoerce

-- order of arguments chosen to be consistent with Data.Exists.runExists
runSubRecord :: forall p r. (forall a a_. Union a a_ p => Record a -> r) -> SubRecord p -> r
runSubRecord = unsafeApplyWithUndefinedInstance

mergeSubRecord :: forall p. Record p -> SubRecord p -> Record p
mergeSubRecord r s = unsafeFromSubRecord (toSubRecord r <> s) -- we know r is full Record, so unsafe coerce back is fine here

unsafeGet :: forall p a. String -> SubRecord p -> Nullable a
unsafeGet = runFn2 unsafeGetImpl

-- TODO: probably makes sense to use Nullable instead of Maybe
-- as in all other places it is treated as Nullable because of
-- cheaper conversion - can just do unsafeCoerce to treat it as record with nullable fields.
get ::
  forall a p trash prop.
  IsSymbol prop =>
  Row.Cons prop a trash p =>
  (SProxy prop) -> SubRecord p -> Nullable a
get prop = unsafeGet (reflectSymbol prop)

-- delete ::
--   forall p r l a.
--   IsSymbol l =>
--   Lacks l r =>
--   Cons l a r p =>
--   SProxy l -> SubRecord p -> SubRecord r
-- -- Doesn't work with "safe" delete and "safe" conversion. Seems like compiler is too stupid to understand
-- -- that all of it matches completely fine (or I'm too stupid to understand the case when it doesn't match).
-- delete l = runSubRecord \s -> unsafeToSubRecord $ RecordUnsafe.unsafeDelete (reflectSymbol l) s
-- inspired by https://github.com/purescript/purescript-prelude/blob/v4.1.1/src/Data/Eq.purs#L85-L104
-- | A class for subrecords where all fields have `Eq` instances, used to implement
-- | the `Eq` instance for subrecords.
class EqSubRecord rowlist row where
  eqSubRecord :: RLProxy rowlist -> SubRecord row -> SubRecord row -> Boolean

instance eqRowNil :: EqSubRecord RL.Nil row where
  eqSubRecord _ _ _ = true

instance eqRowCons ::
  ( EqSubRecord rowlistTail row
  , Row.Cons key focus rowTail row
  , IsSymbol key
  , Eq focus
  ) =>
  EqSubRecord (RL.Cons key focus rowlistTail) row where
  -- TODO: this is not an efficient implementation - instead of only going
  -- thru the props that are actually present in the subrecords, it just goes
  -- over all possible props of the parent record.
  eqSubRecord _ ra rb = (get key ra == get key rb) && tail
    where
    key = SProxy :: SProxy key

    tail = eqSubRecord (RLProxy :: RLProxy rowlistTail) ra rb

-- inspired by https://github.com/purescript/purescript-prelude/blob/v4.1.1/src/Data/Show.purs#L39-L71
-- | A class for subrecords where all fields have `Show` instances, used to
-- | implement the `Show` instance for subrecords.
class ShowSubRecordFields rowlist row where
  showSubRecordFields :: RLProxy rowlist -> SubRecord row -> Array String

instance showSubRecordFieldsNil :: ShowSubRecordFields RL.Nil row where
  showSubRecordFields _ _ = []

instance showSubRecordFieldsCons ::
  ( IsSymbol key
  , ShowSubRecordFields rowlistTail row
  , Show focus
  ) =>
  ShowSubRecordFields (RL.Cons key focus rowlistTail) row where
  -- TODO: this is not an efficient implementation - instead of only going
  -- thru the props that are actually present in the subrecords, it just goes
  -- over all possible props of the parent record.
  showSubRecordFields _ record = case toMaybe focus of
    Nothing -> tail
    Just f -> cons (joinWith ": " [ key, show f ]) tail
    where
    key = reflectSymbol (SProxy :: SProxy key)

    focus = unsafeGet key record :: Nullable focus

    tail = showSubRecordFields (RLProxy :: RLProxy rowlistTail) record

------------------------------------------------- Heterogeneous -------------------------------------------------
-- TODO: fork/copy-paste purescript-heterogeneous ? then we could define all
-- those missing instances for subrecord-record interactions
--
-- unfortunately we have to introduce this newtype wrapper to allow conversion
-- from subrecord to record due to functional dependency in the HMap and HMapWithIndex classes
newtype SRtoR f
  = SRtoR f

instance hmapSRtoRInst ::
  ( RL.RowToList rin rl
  , MapSubRecordToRecordWithIndex rl (ConstMapping f) rin () rout
  ) =>
  HMap (SRtoR f) (SubRecord rin) (Record rout) where
  hmap (SRtoR f) = hmapSRtoR f
else instance hmapSRtoSR ::
  ( RL.RowToList rin rl
  , MapSubRecordWithIndex rl (ConstMapping f) rin rout
  ) =>
  HMap f (SubRecord rin) (SubRecord rout) where
  hmap =
    build
      <<< mapSubRecordWithIndexBuilder (RLProxy :: RLProxy rl)
      <<< ConstMapping

-- unfortunately we cannot have `HMap f (Record rin) (SubRecord rout)` even with
-- newtype wrapper for `f` due to functional dependency in the HMap class and overlapping instances
hmapRtoSR ::
  forall f rin rout.
  HMap f (Record rin) (Record rout) =>
  f -> Record rin -> SubRecord rout
hmapRtoSR f rin = toSubRecord (hmap f rin)

hmapSRtoR ::
  forall rin rl f rout.
  RL.RowToList rin rl =>
  MapSubRecordToRecordWithIndex rl (ConstMapping f) rin () rout =>
  f -> SubRecord rin -> Record rout
hmapSRtoR f rin = Record.Builder.build builder {}
  where
  builder = mapSubRecordToRecordWithIndexBuilder (RLProxy :: RLProxy rl) (ConstMapping f) rin

instance hmapWithIndexSRtoRInst ::
  ( RL.RowToList rin rl
  , MapSubRecordToRecordWithIndex rl f rin () rout
  ) =>
  HMapWithIndex (SRtoR f) (SubRecord rin) (Record rout) where
  hmapWithIndex (SRtoR f) = hmapWithIndexSRtoR f
else instance hmapWithIndexSRtoSR ::
  ( RL.RowToList rin rl
  , MapSubRecordWithIndex rl f rin rout
  ) =>
  HMapWithIndex f (SubRecord rin) (SubRecord rout) where
  hmapWithIndex =
    build
      <<< mapSubRecordWithIndexBuilder (RLProxy :: RLProxy rl)

-- unfortunately we cannot have `HMapWithIndex f prop (Record rin) (SubRecord rout)` even with
-- newtype wrapper for `f` due to functional dependency in the HMapWithIndex class and overlapping instances
hmapWithIndexRtoSR ::
  forall f rin rout.
  HMapWithIndex f (Record rin) (Record rout) =>
  f -> Record rin -> SubRecord rout
hmapWithIndexRtoSR f rin = toSubRecord (hmapWithIndex f rin)

hmapWithIndexSRtoR ::
  forall rin rl f rout.
  RL.RowToList rin rl =>
  MapSubRecordToRecordWithIndex rl f rin () rout =>
  f -> SubRecord rin -> Record rout
hmapWithIndexSRtoR f rin = Record.Builder.build builder {}
  where
  builder = mapSubRecordToRecordWithIndexBuilder (RLProxy :: RLProxy rl) f rin

class MapSubRecordWithIndex (xs :: RowList) f (as :: # Type) (bs :: # Type) | xs f -> bs, xs -> as where
  mapSubRecordWithIndexBuilder :: RLProxy xs -> f -> Builder (SubRecord as) (SubRecord bs)

instance mapSubRecordWithIndexCons ::
  ( IsSymbol sym
  , MappingWithIndex f (SProxy sym) a b
  , MapSubRecordWithIndex rest f as bs'
  , Row.Cons sym a bx bs'
  , Row.Cons sym b bx bs
  ) =>
  MapSubRecordWithIndex (RL.Cons sym a rest) f as bs where
  mapSubRecordWithIndexBuilder _ f =
    modify prop (mappingWithIndex f prop)
      <<< mapSubRecordWithIndexBuilder (RLProxy :: RLProxy rest) f
    where
    prop = SProxy :: SProxy sym

instance mapSubRecordWithIndexNil :: MapSubRecordWithIndex RL.Nil fn as as where
  mapSubRecordWithIndexBuilder _ _ = identity

class MapSubRecordToRecordWithIndex (rl :: RowList) f (row :: # Type) (from :: # Type) (to :: # Type) | rl f row -> to, rl -> from where
  mapSubRecordToRecordWithIndexBuilder :: RLProxy rl -> f -> SubRecord row -> Record.Builder.Builder (Record from) (Record to)

instance mapSubRecordToRecordWithIndexCons ::
  ( IsSymbol sym
  , MappingWithIndex f (SProxy sym) (Nullable a) b
  , MapSubRecordToRecordWithIndex rest f row from from'
  , Row.Lacks sym from'
  , Row.Cons sym b from' to
  , Row.Cons sym a trash row
  ) =>
  MapSubRecordToRecordWithIndex (RL.Cons sym a rest) f row from to where
  mapSubRecordToRecordWithIndexBuilder _ f row =
    Record.Builder.insert prop (mappingWithIndex f prop value)
      <<< rest
    where
    prop = SProxy :: SProxy sym

    value = get prop row

    rest :: Record.Builder.Builder (Record from) (Record from')
    rest = mapSubRecordToRecordWithIndexBuilder (RLProxy :: RLProxy rest) f row

instance mapSubRecordToRecordWithIndexNil :: MapSubRecordToRecordWithIndex RL.Nil f row r r where
  mapSubRecordToRecordWithIndexBuilder _ _ _ = identity

instance hfoldlSubRecord ::
  ( RL.RowToList r rl
  , FoldlSubRecord (ConstFolding f) x rl r b
  ) =>
  HFoldl f x (SubRecord r) b where
  hfoldl f x = foldlSubRecordRowList (ConstFolding f) x (RLProxy :: RLProxy rl)

instance hfoldlSubRecordWithIndex ::
  ( RL.RowToList r rl
  , FoldlSubRecord f x rl r b
  ) =>
  HFoldlWithIndex f x (SubRecord r) b where
  hfoldlWithIndex f x = foldlSubRecordRowList f x (RLProxy :: RLProxy rl)

class FoldlSubRecord f x (rl :: RowList) (r :: # Type) b | f x rl -> b, rl -> r where
  foldlSubRecordRowList :: f -> x -> RLProxy rl -> SubRecord r -> b

instance foldlSubRecordCons ::
  ( IsSymbol sym
  , Row.Cons sym a r' r
  , FoldingWithIndex f (SProxy sym) x (Nullable a) z
  , FoldlSubRecord f z rl r b
  ) =>
  FoldlSubRecord f x (RL.Cons sym a rl) r b where
  foldlSubRecordRowList f x _ r =
    foldlSubRecordRowList f
      (foldingWithIndex f prop x (get prop r))
      (RLProxy :: RLProxy rl)
      r
    where
    prop = SProxy :: SProxy sym

instance foldlSubRecordNil ::
  FoldlSubRecord f x RL.Nil r x where
  foldlSubRecordRowList f x _ r = x

------------------------------------------------- Builder -------------------------------------------------
-- TODO: Is there a way of moving Builder out? The problem is that it is needed to define heterogeneous instances for `SubRecord`.
foreign import copyRecord :: forall r1. SubRecord r1 -> SubRecord r1

foreign import unsafeInsert :: forall a r1 r2. String -> a -> SubRecord r1 -> SubRecord r2

foreign import unsafeModify :: forall a b r1 r2. String -> (a -> b) -> SubRecord r1 -> SubRecord r2

foreign import unsafeDelete :: forall r1 r2. String -> SubRecord r1 -> SubRecord r2

foreign import unsafeRename :: forall r1 r2. String -> String -> SubRecord r1 -> SubRecord r2

foreign import unsafeMerge :: forall r1 r2 r3. SubRecord r1 -> SubRecord r2 -> SubRecord r3

-- | A `Builder` can be used to `build` a subrecord by incrementally adding
-- | fields in-place, instead of using `insert` and repeatedly generating new
-- | immutable subrecords which need to be garbage collected.
-- |
-- | The `Category` instance for `Builder` can be used to compose builders.
-- |
-- | For example:
-- |
-- | ```purescript
-- | build (insert x (Just 42) >>> insert y (Just "testing")) (mkSubRecord {}) :: SubRecord ( x :: Int, y :: String )
-- | ```
newtype Builder a b
  = Builder (a -> b)

-- | Build a record, starting from some other record.
build :: forall r1 r2. Builder (SubRecord r1) (SubRecord r2) -> SubRecord r1 -> SubRecord r2
build (Builder b) r1 = b (copyRecord r1)

derive newtype instance semigroupoidBuilder :: Semigroupoid Builder

derive newtype instance categoryBuilder :: Category Builder

-- | Build by inserting a new field with possibly null value.
insert ::
  forall l a r1 r2.
  Row.Cons l a r1 r2 =>
  Row.Lacks l r1 =>
  IsSymbol l =>
  SProxy l ->
  Nullable a ->
  Builder (SubRecord r1) (SubRecord r2)
insert l a = Builder \r1 -> unsafeInsert (reflectSymbol l) a r1

replace ::
  forall l a a_ r1 r2 r_.
  IsSymbol l =>
  Row.Cons l a_ r_ r1 =>
  Row.Cons l a r_ r2 =>
  SProxy l -> Nullable a -> Builder (SubRecord r1) (SubRecord r2)
replace l a = Builder \r1 -> unsafeInsert (reflectSymbol l) a r1

-- | Build by modifying a potentially existing field.
modify ::
  forall l a b r r1 r2.
  Row.Cons l a r r1 =>
  Row.Cons l b r r2 =>
  IsSymbol l =>
  SProxy l ->
  (a -> b) ->
  Builder (SubRecord r1) (SubRecord r2)
modify l f = Builder \r1 -> unsafeModify (reflectSymbol l) f r1

-- | Build by deleting a potentially existing field.
delete ::
  forall l a r1 r2.
  IsSymbol l =>
  Row.Lacks l r1 =>
  Row.Cons l a r1 r2 =>
  SProxy l ->
  Builder (SubRecord r2) (SubRecord r1)
delete l = Builder \r2 -> unsafeDelete (reflectSymbol l) r2

-- | Build by renaming a potentially existing field.
rename ::
  forall l1 l2 a r1 r2 r3.
  IsSymbol l1 =>
  IsSymbol l2 =>
  Row.Cons l1 a r2 r1 =>
  Row.Lacks l1 r2 =>
  Row.Cons l2 a r2 r3 =>
  Row.Lacks l2 r2 =>
  SProxy l1 ->
  SProxy l2 ->
  Builder (SubRecord r1) (SubRecord r3)
rename l1 l2 = Builder \r1 -> unsafeRename (reflectSymbol l1) (reflectSymbol l2) r1

-- | Build by merging fields from another subrecord.
merge ::
  forall r1 r2 r3.
  Union r1 r2 r3 =>
  SubRecord r2 ->
  Builder (SubRecord r1) (SubRecord r3)
merge r2 = Builder \r1 -> unsafeMerge r1 r2
