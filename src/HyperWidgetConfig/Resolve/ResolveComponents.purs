module HyperWidgetConfig.Resolve.ResolveComponents where

import Data.Nullable (Nullable)
import Data.SubRecord (SubRecord)
import Extra.Data.Row (class HasProp)
import Extra.Debug (class CompileTimeCheck, any)
import Prim.Boolean (False, True, kind Boolean)
import Prim.Row as Row
import Prim.RowList (class RowToList, Nil, kind RowList)
import Prim.RowList as RL

inferResolvedComponents' :: forall from to. CompileTimeCheck => ResolveComponents_ from to => from -> to
inferResolvedComponents' _ = any

isResolvedComponents' :: forall from to. CompileTimeCheck => ResolveComponents_ from to => from -> to -> Boolean
isResolvedComponents' _ _ = true

class ResolveComponents_ from to | from -> to

instance resolveComponentsInst ::
  ( HasProp "component" String from hasComponent
  , RowToList from lfrom
  , ResolveComponentsRowList hasComponent lfrom to
  ) =>
  ResolveComponents_ (Record from) (Record to)

class ResolveComponentsRowList (hasComponent :: Boolean) (lfrom :: RowList) (to :: # Type) | hasComponent lfrom -> to

instance resolveComponentsRowListNil :: ResolveComponentsRowList hasComponent Nil to

instance resolveComponentsRowListFalseR ::
  ( ResolveComponents_ (Record dfrom) (Record dto)
  , Row.Cons prop (Record dto) tto to
  , ResolveComponentsRowList False tfrom tto
  ) =>
  ResolveComponentsRowList
    False
    (RL.Cons prop (Record dfrom) tfrom)
    to
else instance resolveComponentsRowListFalseA ::
  ( Row.Cons prop a tto to
  , ResolveComponentsRowList False tfrom tto
  ) =>
  ResolveComponentsRowList
    False
    (RL.Cons prop a tfrom)
    to

instance resolveComponentsRowListTrueNullable ::
  ResolveComponentsRowList True (RL.Cons prop a tfrom) to =>
  ResolveComponentsRowList
    True
    (RL.Cons prop (Nullable a) tfrom)
    to
else instance resolveComponentsRowListTrueStates ::
  ( Row.Cons "states" states tto to
  , ResolveComponentsRowList True tfrom tto
  ) =>
  ResolveComponentsRowList
    True
    (RL.Cons "states" states tfrom)
    to
else instance resolveComponentsRowListTrueSR ::
  ( HasProp "component" String dfrom dhasComponent
  , RowToList dfrom dlfrom
  , ResolveComponentsRowList dhasComponent dlfrom dto
  , Row.Cons prop (Record dto) tto to
  , ResolveComponentsRowList True tfrom tto
  ) =>
  ResolveComponentsRowList
    True
    (RL.Cons prop (SubRecord dfrom) tfrom)
    to
else instance resolveComponentsRowListTrueA ::
  ( Row.Cons prop a tto to
  , ResolveComponentsRowList True tfrom tto
  ) =>
  ResolveComponentsRowList
    True
    (RL.Cons prop a tfrom)
    to
