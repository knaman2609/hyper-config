module HyperWidgetConfig.Resolve.ResolveStates where

import Prelude
import Data.Maybe (fromMaybe)
import Data.Nullable (Nullable, toMaybe)
import Data.SubRecord (SubRecord, mkSubRecord)
import Data.SubRecord.Heterogeneous.Nullable (class HToNullable, htoNullable)
import Data.Symbol (class IsSymbol, SProxy)
import Extra.Debug (class CompileTimeCheck, any)
import Heterogeneous.Mapping (class HMap, class HMapWithIndex, class MappingWithIndex, hmap, hmapWithIndex)
import Prim.Row as Row
import Record as Record
import Type.Row.Homogeneous (class Homogeneous)

-- TODO: rename resolveStates to propagateStates
inferResolvedStates' :: forall from to. CompileTimeCheck => ResolveStates_ from to => from -> to
inferResolvedStates' _ = any

isResolvedStates' :: forall from to. CompileTimeCheck => ResolveStates_ from to => from -> to -> Boolean
isResolvedStates' _ _ = true

class PropagateStateImmediate env from to | env from -> to where
  propagateStateImmediate :: env -> from -> to

instance propagateStateImmediateSubRecord ::
  ( HToNullable (SubRecord from) (Record nullable)
  , HMapWithIndex
      (PropagateStateImmediateMapping (Record env))
      (Record nullable)
      (Record to)
  ) =>
  PropagateStateImmediate (Record env) (SubRecord from) (Record to) where
  propagateStateImmediate env state = result
    where
    nullable :: Record nullable
    nullable = htoNullable state

    result :: Record to
    result = hmapWithIndex (PropagateStateImmediateMapping env) nullable

newtype PropagateStateImmediateMapping env
  = PropagateStateImmediateMapping env

-- skipping inner states
instance propagateStateImmediateMappingWithIndexStates ::
  MappingWithIndex
    (PropagateStateImmediateMapping env)
    (SProxy "states")
    (Nullable (SubRecord states))
    (SubRecord states) where
  mappingWithIndex _ _ = fromMaybe (mkSubRecord {} :: SubRecord states) <<< toMaybe
-- merging subcomponent props (without states) and props groups (like backgroundStyle)
else instance propagateStateImmediateMappingWithIndexSubRecord ::
  ( IsSymbol prop
  , Row.Cons prop (Record denv) trash env
  , PropagateStateImmediate (Record denv) (SubRecord from) (Record to)
  ) =>
  MappingWithIndex
    (PropagateStateImmediateMapping (Record env))
    (SProxy prop)
    (Nullable (SubRecord from))
    (Record to) where
  mappingWithIndex (PropagateStateImmediateMapping env) prop ns = propagateStateImmediate denv s
    where
    denv = Record.get prop env

    s = fromMaybe (mkSubRecord {} :: SubRecord from) (toMaybe ns)
-- propagate prop from env if its missing in target
else instance propagateStateImmediateMappingWithIndex ::
  ( IsSymbol prop
  , Row.Cons prop a trash env
  ) =>
  MappingWithIndex
    (PropagateStateImmediateMapping (Record env))
    (SProxy prop)
    (Nullable a)
    a where
  mappingWithIndex (PropagateStateImmediateMapping env) prop na = fromMaybe (Record.get prop env) (toMaybe na)

class ResolveStates_ from to | from -> to where
  -- | To resolve states we propagate them down.
  -- | It searches all subrecords with "states" prop, and then for each of those:
  -- | - Applies immediate props to each of the states (recursive on subrecords, but ignoring any other "states" props)
  -- | - Recurses itself on each of the states
  resolveStates' :: from -> to

instance resolveStatesSubRecord ::
  ( HMapWithIndex
        (PropagateStatesMapping (Record from))
        (Record from)
        (Record to)
    ) =>
  ResolveStates_ (Record from) (Record to) where
  resolveStates' config = hmapWithIndex (PropagateStatesMapping config) config

newtype PropagateStatesMapping parent
  = PropagateStatesMapping parent

instance propagateStatesMappingWithIndex ::
  ( Homogeneous states (SubRecord state)
  , PropagateStateImmediate (Record props) (SubRecord state) (Record state')
  , HToNullable (SubRecord states) (Record nullableStates)
  , HMap
      (Nullable (SubRecord state) -> Record state')
      (Record nullableStates)
      (Record states')
  , ResolveStates_ (Record states') (Record states'')
  ) =>
  MappingWithIndex
    (PropagateStatesMapping (Record props))
    (SProxy "states")
    (SubRecord states)
    (Record states'') where
  mappingWithIndex (PropagateStatesMapping props) _ states = propagatedInner
    where
    nullableStates :: Record nullableStates
    nullableStates = htoNullable states

    fn :: Nullable (SubRecord state) -> Record state'
    fn =
      propagateStateImmediate props
        <<< fromMaybe (mkSubRecord {} :: SubRecord state)
        <<< toMaybe

    propagated :: Record states'
    propagated = hmap fn nullableStates

    propagatedInner :: Record states''
    propagatedInner = resolveStates' propagated
else instance propagateStatesMappingWithIndexSubRecord ::
  ResolveStates_ (Record from) (Record to) =>
  MappingWithIndex
    (PropagateStatesMapping env_)
    prop_
    (Record from)
    (Record to) where
  mappingWithIndex _ _ r = resolveStates' r
else instance propagateStatesMappingWithIndexElse ::
  MappingWithIndex
    (PropagateStatesMapping env_)
    prop_
    a
    a where
  mappingWithIndex _ _ = identity
