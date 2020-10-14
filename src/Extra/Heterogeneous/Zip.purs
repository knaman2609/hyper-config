module Extra.Heterogeneous.Zip where

import Prelude
import Data.Nullable (Nullable, null)
import Data.SubRecord (SubRecord)
import Data.SubRecord.Heterogeneous.Nullable (class HToNullable, htoNullable)
import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Tuple (Tuple(..))
import Prim.Row as Row
import Prim.RowList (class RowToList, kind RowList)
import Prim.RowList as RL
import Record as Record
import Record.Builder as Record.Builder
import Type.Data.RowList (RLProxy(..))

class HZipRight left right result | left right -> result where
  -- | For each property of `right` takes a corresponding property in `left` and puts them both
  -- | in a tuple.
  hzipRight :: left -> right -> result

instance hzipRightRandR ::
  ( RowToList left rlLeft
  , RowToList right rlRight
  , ZipRightRecord rlLeft rlRight left right result
  ) =>
  HZipRight (Record left) (Record right) (Record result) where
  hzipRight left = Record.Builder.build builder
    where
    builder = zipRightRecordBuilder (RLProxy :: RLProxy rlLeft) (RLProxy :: RLProxy rlRight) left

instance hzipRightSRandSR ::
  ( RowToList left' rlLeft
  , RowToList right' rlRight
  , HToNullable (SubRecord left) (Record left')
  , HToNullable (SubRecord right) (Record right')
  , ZipRightRecord rlLeft rlRight left' right' result
  ) =>
  HZipRight (SubRecord left) (SubRecord right) (Record result) where
  hzipRight left right = Record.Builder.build builder right'
    where
    left' = htoNullable left

    right' = htoNullable right

    builder = zipRightRecordBuilder (RLProxy :: RLProxy rlLeft) (RLProxy :: RLProxy rlRight) left'

instance hzipRightRandSR ::
  ( RowToList left rlLeft
  , RowToList right' rlRight
  , HToNullable (SubRecord right) (Record right')
  , ZipRightRecord rlLeft rlRight left right' result
  ) =>
  HZipRight (Record left) (SubRecord right) (Record result) where
  hzipRight left right = Record.Builder.build builder right'
    where
    right' = htoNullable right

    builder = zipRightRecordBuilder (RLProxy :: RLProxy rlLeft) (RLProxy :: RLProxy rlRight) left

instance hzipRightSRandR ::
  ( RowToList left' rlLeft
  , RowToList right rlRight
  , HToNullable (SubRecord left) (Record left')
  , ZipRightRecord rlLeft rlRight left' right result
  ) =>
  HZipRight (SubRecord left) (Record right) (Record result) where
  hzipRight left = Record.Builder.build builder
    where
    left' = htoNullable left

    builder = zipRightRecordBuilder (RLProxy :: RLProxy rlLeft) (RLProxy :: RLProxy rlRight) left'

-- can't use HMap because we need a pattern match on the RowList
class ZipRightRecord rlLeft rlRight left from to | rlLeft rlRight left from -> to where
  zipRightRecordBuilder ::
    RLProxy rlLeft ->
    RLProxy rlRight ->
    Record left ->
    Record.Builder.Builder (Record from) (Record to)

instance zipRightRecordBothCons ::
  ( IsSymbol prop
  , ZipRightRecord restLeft restRight left from to'
  , Row.Cons prop l left_ left
  , Row.Cons prop r trash to'
  , Row.Cons prop (Tuple l r) trash to
  ) =>
  ZipRightRecord (RL.Cons prop l restLeft) (RL.Cons prop r restRight) left from to where
  zipRightRecordBuilder _ _ left =
    Record.Builder.modify prop (Tuple (Record.get prop left))
      <<< zipRightRecordBuilder (RLProxy :: RLProxy restLeft) (RLProxy :: RLProxy restRight) left
    where
    prop = SProxy :: SProxy prop
else instance zipRightRecordNullableRightCons ::  -- this instance is to prevent appearance of (Nullable (Nullable r))
  ( IsSymbol prop
  , ZipRightRecord rlLeft restRight left from to'
  , Row.Cons prop (Nullable r) trash to'
  , Row.Cons prop (Tuple (Nullable r) (Nullable r)) trash to
  ) =>
  ZipRightRecord rlLeft (RL.Cons prop (Nullable r) restRight) left from to where
  zipRightRecordBuilder _ _ left =
    Record.Builder.modify prop (Tuple (null :: Nullable r))
      <<< zipRightRecordBuilder (RLProxy :: RLProxy rlLeft) (RLProxy :: RLProxy restRight) left
    where
    prop = SProxy :: SProxy prop
else instance zipRightRecordRightCons ::
  ( IsSymbol prop
  , ZipRightRecord rlLeft restRight left from to'
  , Row.Cons prop r trash to'
  , Row.Cons prop (Tuple (Nullable r) r) trash to
  ) =>
  ZipRightRecord rlLeft (RL.Cons prop r restRight) left from to where
  zipRightRecordBuilder _ _ left =
    Record.Builder.modify prop (Tuple (null :: Nullable r))
      <<< zipRightRecordBuilder (RLProxy :: RLProxy rlLeft) (RLProxy :: RLProxy restRight) left
    where
    prop = SProxy :: SProxy prop

instance zipRightRecordNil :: ZipRightRecord rlLeft RL.Nil left r r where
  zipRightRecordBuilder _ _ _ = identity
