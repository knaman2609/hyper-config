module HyperWidgetConfig.Resolve.ResolveAll where

import Data.SubRecord (SubRecord)
import Extra.Debug (class CompileTimeCheck, any)
import HyperWidgetConfig.Reference (Reference)
import Prim.RowList (class RowToList, Cons, Nil, kind RowList)
import Type.RowList (class ListToRow)

inferResolved' :: forall from to. CompileTimeCheck => Resolve_ from to => from -> to
inferResolved' _ = any

isResolved' :: forall from to. CompileTimeCheck => Resolve_ from to => from -> to -> Boolean
isResolved' _ _ = true

-- | Typeclass to "strip" `Reference` from record value types and to "enforce" SubRecords up to Records.
-- | For example, it converts
-- | ```
-- | ( someKey :: Reference Int
-- | , someComponent :: SubRecord ( someComponentProp :: Reference String )
-- | )
-- | ```
-- | into
-- | ```
-- | ( someKey :: Int
-- | , someComponent :: Record ( someComponentProp :: String )
-- | )
-- | ```
-- | recursively, both for Records and SubRecords, skipping non-Reference values as-is.
class Resolve_ c r | c -> r

instance resolveSR ::
  ( RowToList c lc
  , RowToList r lr
  , ResolveRowList lc lr
  , ListToRow lc c
  , ListToRow lr r
  ) =>
  Resolve_ (SubRecord c) (Record r)

instance resolveR ::
  ( RowToList c lc
  , RowToList r lr
  , ResolveRowList lc lr
  , ListToRow lc c
  , ListToRow lr r
  ) =>
  Resolve_ (Record c) (Record r)

class ResolveRowList (c :: RowList) (r :: RowList) | c -> r

instance resolveRowListNil :: ResolveRowList Nil Nil

instance resolveRowListConsReference ::
  ResolveRowList tc tr =>
  ResolveRowList
    (Cons k (Reference v) tc)
    (Cons k v tr)
else instance resolveRowListConsRecord ::
  ( ResolveRowList tc tr
  , Resolve_ (SubRecord ic) (Record ir)
  ) =>
  ResolveRowList
    (Cons k (SubRecord ic) tc)
    (Cons k (Record ir) tr)
else instance resolveRowListConsSubRecord ::
  ( ResolveRowList tc tr
  , Resolve_ (Record ic) (Record ir)
  ) =>
  ResolveRowList
    (Cons k (Record ic) tc)
    (Cons k (Record ir) tr)
else instance resolveRowListCons ::
  ResolveRowList tc tr =>
  ResolveRowList
    (Cons k v tc)
    (Cons k v tr)
