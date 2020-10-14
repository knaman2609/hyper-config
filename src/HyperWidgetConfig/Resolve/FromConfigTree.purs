module HyperWidgetConfig.Resolve.FromConfigTree (fromResolvedComponentsConfigRoot) where

import Prelude
import Data.Nullable (toNullable)
import Foreign.Object (Object)
import Foreign.Object as Object
import HyperWidgetConfig.Config as Config
import HyperWidgetConfig.Resolve.ConfigTree.ResolveComponents (ResolvedComponentsConfigRoot, toConfigRoot)
import HyperWidgetConfig.Resolve.ConfigTree.Types
  ( ComponentDefinition(..)
  , ComponentReference(..)
  , ConfigRoot(..)
  , ConfigTree(..)
  , Props
  , PropsTree(..)
  , Section
  , StateProps(..)
  , States
  , SubState(..)
  , ComponentReferenceValue
  )
import HyperWidgetConfig.Resolve.ConfigTree.Value (Any, mkAny, unsafeFromValue)
import Unsafe.Coerce (unsafeCoerce)

fromResolvedComponentsConfigRoot :: ResolvedComponentsConfigRoot -> Config.ResolvedComponentsProps
-- TODO: better to implement it in "heterogeneous" way
fromResolvedComponentsConfigRoot rcr = unsafeCoerce result
  where
  cr = toConfigRoot rcr

  result = fromConfigRoot cr

fromConfigRoot :: ConfigRoot -> Object Any
fromConfigRoot (ConfigRoot section) = fromSection section

fromSection :: Section -> Object Any
fromSection s = fromConfigTree <$> s

fromConfigTree :: ConfigTree -> Any
fromConfigTree (Section section) = mkAny $ fromSection section

fromConfigTree (Component cd) = mkAny $ fromComponentDefinition cd

fromConfigTree (Value v) = unsafeFromValue v

fromComponentDefinition :: ComponentDefinition -> Object Any
fromComponentDefinition (ComponentDefinition cd) =
  Object.unions
    [ fromProps cd.props
    , Object.fromHomogeneous
        { states: mkAny $ fromStates cd.states
        }
    , fromComponentReferences cd.subComponents
    ]

fromComponentReference :: ComponentReference -> Object Any
fromComponentReference (ComponentReference cr) =
  Object.unions
    [ fromProps cr.props
    , Object.fromHomogeneous
        { states: mkAny $ fromStates cr.states
        , component: fromComponentReferenceValue cr.component
        }
    , fromComponentReferences cr.subComponents
    ]

fromProps :: Props -> Object Any
fromProps ps = fromPropsTree <$> ps

fromPropsTree :: PropsTree -> Any
fromPropsTree (Prop v) = unsafeFromValue v

fromPropsTree (Group g) = mkAny $ fromProps g

fromStates :: States -> Object Any
fromStates s = mkAny <<< fromStateProps <$> s

fromStateProps :: StateProps -> Object Any
fromStateProps (StateProps sp) =
  Object.unions
    [ fromProps sp.props
    , fromSubStates sp.subComponents
    ]

fromSubStates :: Object SubState -> Object Any
fromSubStates s = mkAny <<< fromSubState <$> s

fromSubState :: SubState -> Object Any
fromSubState (SubState ss) =
  Object.unions
    [ fromProps ss.props
    , Object.fromHomogeneous { states: mkAny $ fromStates ss.states }
    , fromSubStates ss.subComponents
    ]

fromComponentReferences :: Object ComponentReference -> Object Any
fromComponentReferences crs = mkAny <<< fromComponentReference <$> crs

fromComponentReferenceValue :: ComponentReferenceValue -> Any
fromComponentReferenceValue { reference } = mkAny (toNullable reference)
