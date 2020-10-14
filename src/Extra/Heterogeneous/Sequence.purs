module Extra.Heterogeneous.Sequence where

import Prelude
import Control.Apply (lift2)
import Data.Nullable (Nullable, toMaybe, toNullable)
import Data.SubRecord (SubRecord, mkSubRecord)
import Data.SubRecord.Builder as SubRecord.Builder
import Data.Symbol (class IsSymbol, SProxy)
import Data.Traversable (sequence, traverse)
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Prim.Row as Row
import Record.Builder as Record.Builder

class
  Applicative f <= HSequence f rin rout | f rin -> rout where
  -- | Recursively accumulates Applicative values.
  hsequenceRecursive :: rin -> f rout

instance hsequenceRecursiveRecord ::
  ( Applicative f
  , HFoldlWithIndex
      SequenceFolding
      (f (Record.Builder.Builder {} {}))
      (Record rin)
      (f (Record.Builder.Builder {} (Record rout)))
  ) =>
  HSequence f (Record rin) (Record rout) where
  hsequenceRecursive rin = Record.Builder.build <@> {} <$> builder
    where
    builder = hfoldlWithIndex SequenceFolding (pure identity :: f (Record.Builder.Builder {} {})) rin

instance hsequenceRecursiveSubRecord ::
  ( Applicative f
  , HFoldlWithIndex
      SequenceFolding
      (f (SubRecord.Builder.Builder (SubRecord ()) (SubRecord ())))
      (SubRecord rin)
      (f (SubRecord.Builder.Builder (SubRecord ()) (SubRecord rout)))
  ) =>
  HSequence f (SubRecord rin) (SubRecord rout) where
  hsequenceRecursive rin = SubRecord.Builder.build <@> mkSubRecord {} <$> builder
    where
    builder = hfoldlWithIndex SequenceFolding (pure identity :: f (SubRecord.Builder.Builder (SubRecord ()) (SubRecord ()))) rin

data SequenceFolding
  = SequenceFolding

-- for records
instance sequenceFoldingRecord ::
  ( IsSymbol prop
  , Apply f
  , Row.Cons prop a rout' rout
  , Row.Lacks prop rout'
  ) =>
  FoldingWithIndex
    SequenceFolding
    (SProxy prop)
    (f (Record.Builder.Builder (Record rin) (Record rout')))
    (f a)
    (f (Record.Builder.Builder (Record rin) (Record rout))) where
  foldingWithIndex _ prop facc fa = lift2 (<<<) first facc
    where
    first = Record.Builder.insert prop <$> fa
else instance sequenceFoldingRecordRecord ::
  ( HSequence f (Record r) (Record r')
  , IsSymbol prop
  , Row.Cons prop (Record r') rout' rout
  , Row.Lacks prop rout'
  ) =>
  FoldingWithIndex
    SequenceFolding
    (SProxy prop)
    (f (Record.Builder.Builder (Record rin) (Record rout')))
    (Record r)
    (f (Record.Builder.Builder (Record rin) (Record rout))) where
  foldingWithIndex _ prop facc r = lift2 (<<<) first facc
    where
    first = Record.Builder.insert prop <$> hsequenceRecursive r
else instance sequenceFoldingRecordSubRecord ::
  ( HSequence f (SubRecord r) (SubRecord r')
  , IsSymbol prop
  , Row.Cons prop (SubRecord r') rout' rout
  , Row.Lacks prop rout'
  ) =>
  FoldingWithIndex
    SequenceFolding
    (SProxy prop)
    (f (Record.Builder.Builder (Record rin) (Record rout')))
    (SubRecord r)
    (f (Record.Builder.Builder (Record rin) (Record rout))) where
  foldingWithIndex _ prop facc r = lift2 (<<<) first facc
    where
    first = Record.Builder.insert prop <$> hsequenceRecursive r
else instance sequenceFoldingRecordLoosely ::
  ( Applicative f
  , IsSymbol prop
  , Row.Cons prop a rout' rout
  , Row.Lacks prop rout'
  ) =>
  FoldingWithIndex
    SequenceFolding
    (SProxy prop)
    (f (Record.Builder.Builder (Record rin) (Record rout')))
    a
    (f (Record.Builder.Builder (Record rin) (Record rout))) where
  foldingWithIndex _ prop facc a = (<<<) first <$> facc
    where
    first = Record.Builder.insert prop a

-- for subrecords
instance sequenceFoldingSubRecord ::
  ( IsSymbol prop
  , Applicative f
  , Row.Cons prop a rout' rout
  , Row.Lacks prop rout'
  ) =>
  FoldingWithIndex
    SequenceFolding
    (SProxy prop)
    (f (SubRecord.Builder.Builder (SubRecord rin) (SubRecord rout')))
    (Nullable (f a))
    (f (SubRecord.Builder.Builder (SubRecord rin) (SubRecord rout))) where
  foldingWithIndex _ prop facc nfa = lift2 (<<<) first facc
    where
    first = (SubRecord.Builder.insert prop <<< toNullable) <$> (sequence <<< toMaybe) nfa
else instance sequenceFoldingSubRecordRecord ::
  ( HSequence f (Record r) (Record r')
  , IsSymbol prop
  , Row.Cons prop (Record r') rout' rout
  , Row.Lacks prop rout'
  ) =>
  FoldingWithIndex
    SequenceFolding
    (SProxy prop)
    (f (SubRecord.Builder.Builder (SubRecord rin) (SubRecord rout')))
    (Nullable (Record r))
    (f (SubRecord.Builder.Builder (SubRecord rin) (SubRecord rout))) where
  foldingWithIndex _ prop facc nr = lift2 (<<<) first facc
    where
    first = (SubRecord.Builder.insert prop <<< toNullable) <$> traverse hsequenceRecursive (toMaybe nr)
else instance sequenceFoldingSubRecordSubRecord ::
  ( HSequence f (SubRecord r) (SubRecord r')
  , IsSymbol prop
  , Row.Cons prop (SubRecord r') rout' rout
  , Row.Lacks prop rout'
  ) =>
  FoldingWithIndex
    SequenceFolding
    (SProxy prop)
    (f (SubRecord.Builder.Builder (SubRecord rin) (SubRecord rout')))
    (Nullable (SubRecord r))
    (f (SubRecord.Builder.Builder (SubRecord rin) (SubRecord rout))) where
  foldingWithIndex _ prop facc nr = lift2 (<<<) first facc
    where
    first = (SubRecord.Builder.insert prop <<< toNullable) <$> traverse hsequenceRecursive (toMaybe nr)
else instance sequenceFoldingSubRecordLoosely ::
  ( Applicative f
  , IsSymbol prop
  , Row.Cons prop a rout' rout
  , Row.Lacks prop rout'
  ) =>
  FoldingWithIndex
    SequenceFolding
    (SProxy prop)
    (f (SubRecord.Builder.Builder (SubRecord rin) (SubRecord rout')))
    (Nullable a)
    (f (SubRecord.Builder.Builder (SubRecord rin) (SubRecord rout))) where
  foldingWithIndex _ prop facc na = (<<<) first <$> facc
    where
    first = SubRecord.Builder.insert prop na
