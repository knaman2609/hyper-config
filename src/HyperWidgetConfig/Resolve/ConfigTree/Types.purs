module HyperWidgetConfig.Resolve.ConfigTree.Types where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (IndexedTraversal', Lens', itraverseOf)
import Data.Lens.Indexed (itraversed, iwander)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Extra.Data.Lens.Indexed (ReversedSegments, icombineS, isegmentify)
import Foreign.Object (Object)
import HyperWidgetConfig.Resolve.ConfigTree.Value (Value)

type Section
  = Object ConfigTree

type Props
  = Object PropsTree

type Group
  = Object PropsTree

data PropsTree
  = Group Group
  | Prop Value

derive instance eqPropsTree :: Eq PropsTree

-- can't use genericShow because of recursive data structure
instance showPropsTree :: Show PropsTree where
  show (Group group) = "(Group " <> show group <> ")"
  show (Prop prop) = "(Prop " <> show prop <> ")"

newtype StateProps
  = StateProps
  { props :: Props
  , subComponents :: Object SubState
  }

derive instance eqStateProps :: Eq StateProps

-- can't use genericShow because of recursive data structure
instance showStateProps :: Show StateProps where
  show (StateProps state) = "(StateProps " <> show state <> ")"

newtype SubState
  = SubState
  { props :: Props
  , subComponents :: Object SubState
  , states :: States
  }

derive instance eqSubState :: Eq SubState

-- can't use genericShow because of recursive data structure
instance showSubState :: Show SubState where
  show (SubState substate) = "(SubState " <> show substate <> ")"

type States
  = Object StateProps

-- TODO: move identifier level up, like in ComponentDefinition
type ComponentReferenceValue
  = { identifier :: String -- should match ComponentDefinition.identifier
    , reference :: Maybe String
    }

newtype ComponentReference
  = ComponentReference
  { component :: ComponentReferenceValue
  , props :: Props
  , states :: States
  , subComponents :: Object ComponentReference
  }

derive instance newtypeComponentReference :: Newtype ComponentReference _

derive instance eqComponentReference :: Eq ComponentReference

-- can't use genericShow because of recursive data structure
instance showComponentReference :: Show ComponentReference where
  show (ComponentReference cr) = "(ComponentReference " <> show cr <> ")"

newtype ComponentDefinition
  = ComponentDefinition
  { props :: Props
  , states :: States
  , subComponents :: Object ComponentReference
  , identifier :: String -- ComponentReferenceValue.identifier should match this identifier
  }

_identifier :: Lens' ComponentDefinition String
_identifier = _Newtype <<< prop (SProxy :: SProxy "identifier")

derive instance newtypeComponentDefinition :: Newtype ComponentDefinition _

derive instance eqComponentDefinition :: Eq ComponentDefinition

derive instance genericComponentDefinition :: Generic ComponentDefinition _

instance showComponentDefinition :: Show ComponentDefinition where
  show = genericShow

data ConfigTree
  = Section Section
  | Component ComponentDefinition
  | Value Value

derive instance eqConfigTree :: Eq ConfigTree

-- can't use genericShow because of recursive data structure
instance showConfigTree :: Show ConfigTree where
  show (Section section) = "(Section " <> show section <> ")"
  show (Component cd) = "(Component " <> show cd <> ")"
  show (Value v) = "(Value " <> show v <> ")"

newtype ConfigRoot
  = ConfigRoot Section

derive instance newtypeConfigRoot :: Newtype ConfigRoot _

derive instance genericConfigRoot :: Generic ConfigRoot _

derive instance eqConfigRoot :: Eq ConfigRoot

instance showConfigRoot :: Show ConfigRoot where
  show = genericShow

_sectionConfigTrees :: IndexedTraversal' ReversedSegments Section ConfigTree
_sectionConfigTrees = isegmentify itraversed

_sectionComponentDefinitions :: IndexedTraversal' ReversedSegments Section ComponentDefinition
_sectionComponentDefinitions = icombineS _sectionConfigTrees _configTreeComponentDefinitions

_configTreeComponentDefinitions :: IndexedTraversal' ReversedSegments ConfigTree ComponentDefinition
_configTreeComponentDefinitions = iwander itra
  where
  itra :: forall f. Applicative f => (ReversedSegments -> ComponentDefinition -> f ComponentDefinition) -> ConfigTree -> f ConfigTree
  itra f v@(Value _) = pure v

  itra f (Component cd) = Component <$> f mempty cd

  itra f (Section s) = Section <$> itraverseOf _sectionComponentDefinitions f s

_componentDefinitions :: IndexedTraversal' ReversedSegments ConfigRoot ComponentDefinition
_componentDefinitions = _Newtype <<< _sectionComponentDefinitions
