module Extra.Data.Row where

import Prelude
import Data.Symbol (class IsSymbol, SProxy)
import Prim.Boolean (False, True, kind Boolean)
import Prim.Row (class Union)
import Prim.Row as Row
import Prim.RowList (class RowToList, Nil, kind RowList)
import Prim.RowList as RL
import Record.Builder (Builder)
import Record.Builder as Builder

data NotAType
  = NotAType

class MaybeConsBuilder prop v row result | prop v row -> result where
  maybeConsBuilder :: (SProxy prop) -> v -> Builder (Record row) (Record result)

instance maybeConsBuilderNotAType :: MaybeConsBuilder prop NotAType row row where
  maybeConsBuilder _ _ = identity
else instance maybeConsBuilderInst ::
  ( IsSymbol prop
  , Row.Cons prop v row result
  , Row.Lacks prop row
  ) =>
  MaybeConsBuilder prop v row result where
  maybeConsBuilder prop v = Builder.insert prop v

class ToRow from (to :: # Type) | from -> to

instance toRowF :: ToRow (f row) row

class HasPropAny (prop :: Symbol) (row :: # Type) (hasProp :: Boolean) | prop row -> hasProp

instance hasPropAny ::
  ( RowToList row rl
  , HasPropAnyRowList prop rl hasProp
  ) =>
  HasPropAny prop row hasProp

class HasPropAnyRowList (prop :: Symbol) (rl :: RowList) (hasProp :: Boolean) | prop rl -> hasProp

instance hasPropAnyRowListNil :: HasPropAnyRowList prop Nil False

instance hasPropAnyRowListHas ::
  HasPropAnyRowList prop (RL.Cons prop v tail) True
else instance hasPropAnyRowListCons ::
  HasPropAnyRowList prop tail hasProp =>
  HasPropAnyRowList prop (RL.Cons prop' v' tail) hasProp

class HasProp (prop :: Symbol) (v :: Type) (row :: # Type) (hasProp :: Boolean) | prop v row -> hasProp

instance hasProp ::
  ( RowToList row rl
  , HasPropRowList prop v rl hasProp
  ) =>
  HasProp prop v row hasProp

class HasPropRowList (prop :: Symbol) (v :: Type) (rl :: RowList) (hasProp :: Boolean) | prop v rl -> hasProp

instance hasPropRowListNil :: HasPropRowList prop v Nil False

instance hasPropRowListHas ::
  HasPropRowList prop v (RL.Cons prop v tail) True
else instance hasPropRowListCons ::
  HasPropRowList prop v tail hasProp =>
  HasPropRowList prop v (RL.Cons prop' v' tail) hasProp

class OptionalPropIf (pred :: Boolean) (prop :: Symbol) (from :: # Type) (to :: # Type) | pred prop from -> to

instance optionalPropIfFalse :: OptionalPropIf False prop r r

instance optionalPropIfTrue ::
  ( Row.Cons prop v required from
  , Row.Cons prop v () optional
  , Union rest trash optional
  , Union required rest to
  ) =>
  OptionalPropIf True prop from to
