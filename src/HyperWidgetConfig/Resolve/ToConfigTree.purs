module HyperWidgetConfig.Resolve.ToConfigTree where

import Prelude
import Data.Array (mapMaybe)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), fromJust, fromMaybe, maybe)
import Data.Nullable (Nullable, toMaybe)
import Data.SubRecord (SRtoR(..), SubRecord, mkSubRecord)
import Data.SubRecord as SubRecord
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Extra.Data.Lens.Indexed (ReversedSegments, addSegment, toPath)
import Extra.Data.Row (class HasProp, class HasPropAny, NotAType(..))
import Foreign.Object (Object)
import Foreign.Object as Object
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, foldingWithIndex, hfoldlWithIndex)
import Heterogeneous.Mapping (class HMap, class Mapping, hmap)
import HyperWidgetConfig.Reify (class Reify)
import HyperWidgetConfig.Resolve.ConfigTree.Types (ComponentDefinition(..), ComponentReference(..), ConfigTree(..), Group, Props, PropsTree(..), Section, StateProps(..), States, SubState(..))
import HyperWidgetConfig.Resolve.ConfigTree.Value (Value, mkValue)
import Partial.Unsafe (unsafePartial)
import Prim.Boolean (False, True, kind Boolean)
import Prim.Row as Row
import Prim.RowList (kind RowList)
import Prim.Symbol (class Append)
import Record as Record
import Type.Data.Boolean (class And, class IsBoolean, class Not, BProxy(..))
import Type.Row.Homogeneous (class Homogeneous)

identifierMap :: Object String
identifierMap =
  Object.fromHomogeneous
    { "components.primaryButton": "components.primaryButton"
    , "containers.gridItem": "containers.gridItem"
    , "containers.gridItem.primaryButton": "components.primaryButton"
    , "screens.someScreen": "screens.someScreen"
    , "screens.someScreen.primaryButton": "components.primaryButton"
    , "screens.someScreen.gridItem": "containers.gridItem"
    , "screens.someScreen.gridItem.primaryButton": "components.primaryButton" -- optional
    }

-- TODO: rethink approach to identifier fetch
-- and at least add compile-time check that identifierMap is correct
getIdentifierByPath :: ReversedSegments -> String
getIdentifierByPath = unsafePartial fromJust <<< flip Object.lookup identifierMap <<< toPath

------------------------------ helpers ------------------------------
class MaybeMap s t a b | s a b -> t where
  maybeMap :: (a -> b) -> s -> t

instance maybeMapNotAType :: MaybeMap NotAType NotAType a b where
  maybeMap _ = identity
else instance maybeMapA :: MaybeMap a b a b where
  maybeMap f v = f v

class MaybeFrom s a b where
  maybeFrom :: b -> (a -> b) -> s -> b

instance maybeFromNotAType :: MaybeFrom NotAType a b where
  maybeFrom b _ _ = b
else instance maybeFromA :: MaybeFrom a a b where
  maybeFrom _ f v = f v

------------------------------ conditionals ------------------------------
class IsComponentDefinition (row :: # Type) (isComponentDefinition :: Boolean)

instance isComponentDefinition ::
  ( HasPropAny "states" row hasStates
  , HasProp "component" String row hasComponent
  , Not hasComponent hasntComponent
  , And hasStates hasntComponent isComponentDefinition
  ) =>
  IsComponentDefinition row isComponentDefinition

class IsComponentReference (row :: # Type) (isComponentReference :: Boolean)

instance isComponentReference ::
  ( HasPropAny "states" row hasStates
  , HasProp "component" String row hasComponent
  , And hasStates hasComponent isComponentReference
  ) =>
  IsComponentReference row isComponentReference

class IsSubState (row :: # Type) (isSubState :: Boolean)

instance isSubState ::
  ( HasPropAny "states" row hasStates
  , HasProp "component" String row hasComponent
  , Not hasComponent hasntComponent
  , And hasStates hasntComponent isSubState
  ) =>
  IsSubState row isSubState

class IsGroup (row :: # Type) (isProps :: Boolean)

instance isGroup ::
  ( HasPropAny "states" row hasStates
  , HasProp "component" String row hasComponent
  , Not hasStates hasntStates
  , Not hasComponent hasntComponent
  , And hasntStates hasntComponent isGroup
  ) =>
  IsGroup row isGroup

class IsSection (row :: # Type) (isProps :: Boolean)

instance isSection ::
  ( HasPropAny "states" row hasStates
  , HasProp "component" String row hasComponent
  , Not hasStates hasntStates
  , Not hasComponent hasntComponent
  , And hasntStates hasntComponent isSection
  ) =>
  IsSection row isSection

------------------------------ orchestrating ------------------------------
class ToConfigTree from where
  toConfigTree :: from -> ReversedSegments -> ConfigTree

instance toConfigTreeR ::
  ( IsSection from isSection
  , IsComponentDefinition from isComponentDefinition
  , ToConfigTree_ isSection isComponentDefinition (Record from)
  ) =>
  ToConfigTree (Record from) where
  toConfigTree = toConfigTree_ (BProxy :: BProxy isSection) (BProxy :: BProxy isComponentDefinition)
else instance toConfigTreeSR ::
  ( IsSection from isSection
  , IsComponentDefinition from isComponentDefinition
  , ToConfigTree_ isSection isComponentDefinition (SubRecord from)
  ) =>
  ToConfigTree (SubRecord from) where
  toConfigTree = toConfigTree_ (BProxy :: BProxy isSection) (BProxy :: BProxy isComponentDefinition)
else instance toConfigTreeA ::
  ToConfigTree_ False False from =>
  ToConfigTree from where
  toConfigTree = toConfigTree_ (BProxy :: BProxy False) (BProxy :: BProxy False)

class ToConfigTree_ (isSection :: Boolean) (isComponentDefinition :: Boolean) from where
  toConfigTree_ :: BProxy isSection -> BProxy isComponentDefinition -> from -> ReversedSegments -> ConfigTree

instance toConfigTreeSection ::
  ToSection from =>
  ToConfigTree_ True False from where
  toConfigTree_ _ _ from segments = Section $ toSection from segments

instance toConfigTreeComponentDefinition ::
  ToComponentDefinition from =>
  ToConfigTree_ False True from where
  toConfigTree_ _ _ from segments = Component $ toComponentDefinition from segments

instance toConfigTreeValue ::
  Reify from =>
  ToConfigTree_ False False from where
  toConfigTree_ _ _ from _ = Value $ toValue from

class MaybeToGroup (isGroup :: Boolean) from to | isGroup from -> to where
  maybeToGroup :: (BProxy isGroup) -> from -> to

instance maybeToGroupFalse :: MaybeToGroup False from NotAType where
  maybeToGroup _ _ = NotAType

instance maybeToGroupTrue ::
  ToGroup from =>
  MaybeToGroup True from (Object PropsTree) where
  maybeToGroup _ = toGroup

class ToProps from where
  toProps :: from -> Props

instance toPropsR ::
  HFoldlWithIndex
      ToPropsFolding
      (List (Tuple String PropsTree))
      (Record from)
      (List (Tuple String PropsTree)) =>
  ToProps (Record from) where
  toProps from = Object.fromFoldable l
    where
    l = hfoldlWithIndex ToPropsFolding (Nil :: List (Tuple String PropsTree)) from

instance toPropsSR ::
  HFoldlWithIndex
      ToPropsFolding
      (List (Tuple String PropsTree))
      (SubRecord from)
      (List (Tuple String PropsTree)) =>
  ToProps (SubRecord from) where
  toProps from = Object.fromFoldable l
    where
    l = hfoldlWithIndex ToPropsFolding (Nil :: List (Tuple String PropsTree)) from

data ToPropsFolding
  = ToPropsFolding

instance toPropsStates ::
  FoldingWithIndex
    ToPropsFolding
    (SProxy "states")
    (List (Tuple String PropsTree))
    v
    (List (Tuple String PropsTree)) where
  foldingWithIndex _ _ acc _ = acc
else instance toPropsComponent ::
  FoldingWithIndex
    ToPropsFolding
    (SProxy "component")
    (List (Tuple String PropsTree))
    v
    (List (Tuple String PropsTree)) where
  foldingWithIndex _ _ acc _ = acc
else instance toPropsNullable ::
  ( IsSymbol prop
  , FoldingWithIndex
      ToPropsFolding
      (SProxy prop)
      (List (Tuple String PropsTree))
      v
      (List (Tuple String PropsTree))
  ) =>
  FoldingWithIndex
    ToPropsFolding
    (SProxy prop)
    (List (Tuple String PropsTree))
    (Nullable v)
    (List (Tuple String PropsTree)) where
  foldingWithIndex f prop acc nv = case toMaybe nv of
    Nothing -> acc
    Just v -> foldingWithIndex f prop acc v
else instance toPropsFoldingR ::
  ( IsSymbol prop
  , IsGroup v isGroup
  , MaybeToGroup isGroup (Record v) v'
  , MaybeFrom v' Group (Maybe PropsTree)
  ) =>
  FoldingWithIndex
    ToPropsFolding
    (SProxy prop)
    (List (Tuple String PropsTree))
    (Record v)
    (List (Tuple String PropsTree)) where
  foldingWithIndex _ prop acc v = case v'' of
    Nothing -> acc
    Just value -> Tuple k value : acc
    where
    k = reflectSymbol prop

    v' = maybeToGroup (BProxy :: BProxy isGroup) v

    v'' = maybeFrom Nothing (Just <<< Group) v'
else instance toPropsFoldingSR ::
  ( IsSymbol prop
  , IsGroup v isGroup
  , MaybeToGroup isGroup (SubRecord v) v'
  , MaybeFrom v' Group (Maybe PropsTree)
  ) =>
  FoldingWithIndex
    ToPropsFolding
    (SProxy prop)
    (List (Tuple String PropsTree))
    (SubRecord v)
    (List (Tuple String PropsTree)) where
  foldingWithIndex _ prop acc v = case v'' of
    Nothing -> acc
    Just value -> Tuple k value : acc
    where
    k = reflectSymbol prop

    v' = maybeToGroup (BProxy :: BProxy isGroup) v

    v'' = maybeFrom Nothing (Just <<< Group) v'
else instance toPropsFoldingA ::
  ( IsSymbol prop
  , Reify v
  ) =>
  FoldingWithIndex
    ToPropsFolding
    (SProxy prop)
    (List (Tuple String PropsTree))
    v
    (List (Tuple String PropsTree)) where
  foldingWithIndex _ prop acc v = Tuple k v' : acc
    where
    k = reflectSymbol prop

    v' = Prop $ mkValue v

class MaybeToSubState (isSubState :: Boolean) from to | isSubState from -> to where
  maybeToSubState :: (BProxy isSubState) -> from -> to

instance maybeToSubStateFalse :: MaybeToSubState False from NotAType where
  maybeToSubState _ _ = NotAType

instance maybeToSubStateTrue ::
  ToSubState from =>
  MaybeToSubState True from SubState where
  maybeToSubState _ = toSubState

class ToSubStates from where
  toSubStates :: from -> Object SubState

instance toSubStatesR ::
  HFoldlWithIndex
      ToSubStatesFolding
      (List (Tuple String SubState))
      (Record from)
      (List (Tuple String SubState)) =>
  ToSubStates (Record from) where
  toSubStates from = Object.fromFoldable l
    where
    l = hfoldlWithIndex ToSubStatesFolding (Nil :: List (Tuple String SubState)) from

instance toSubStatesSR ::
  HFoldlWithIndex
      ToSubStatesFolding
      (List (Tuple String SubState))
      (SubRecord from)
      (List (Tuple String SubState)) =>
  ToSubStates (SubRecord from) where
  toSubStates from = Object.fromFoldable l
    where
    l = hfoldlWithIndex ToSubStatesFolding (Nil :: List (Tuple String SubState)) from

data ToSubStatesFolding
  = ToSubStatesFolding

instance toSubStatesStates ::
  FoldingWithIndex
    ToSubStatesFolding
    (SProxy "states")
    (List (Tuple String SubState))
    v
    (List (Tuple String SubState)) where
  foldingWithIndex _ _ acc _ = acc
else instance toSubStatesNullable ::
  FoldingWithIndex
      ToSubStatesFolding
      (SProxy prop)
      (List (Tuple String SubState))
      v
      (List (Tuple String SubState)) =>
  FoldingWithIndex
    ToSubStatesFolding
    (SProxy prop)
    (List (Tuple String SubState))
    (Nullable v)
    (List (Tuple String SubState)) where
  foldingWithIndex f prop acc nv = case toMaybe nv of
    Nothing -> acc
    Just v -> foldingWithIndex f prop acc v
else instance toSubStatesFoldingR ::
  ( IsSymbol prop
  , IsSubState v isSubState
  , MaybeToSubState isSubState (Record v) v'
  , MaybeFrom v' SubState (Maybe SubState)
  ) =>
  FoldingWithIndex
    ToSubStatesFolding
    (SProxy prop)
    (List (Tuple String SubState))
    (Record v)
    (List (Tuple String SubState)) where
  foldingWithIndex _ prop acc v = case v'' of
    Nothing -> acc
    Just value -> Tuple k value : acc
    where
    k = reflectSymbol prop

    v' = maybeToSubState (BProxy :: BProxy isSubState) v

    v'' = maybeFrom Nothing Just v'
else instance toSubStatesFoldingSR ::
  ( IsSymbol prop
  , IsSubState v isSubState
  , MaybeToSubState isSubState (SubRecord v) v'
  , MaybeFrom v' SubState (Maybe SubState)
  ) =>
  FoldingWithIndex
    ToSubStatesFolding
    (SProxy prop)
    (List (Tuple String SubState))
    (SubRecord v)
    (List (Tuple String SubState)) where
  foldingWithIndex _ prop acc v = case v'' of
    Nothing -> acc
    Just value -> Tuple k value : acc
    where
    k = reflectSymbol prop

    v' = maybeToSubState (BProxy :: BProxy isSubState) v

    v'' = maybeFrom Nothing Just v'
else instance toSubStatesFoldingA ::
  FoldingWithIndex
    ToSubStatesFolding
    (SProxy prop)
    (List (Tuple String SubState))
    v
    (List (Tuple String SubState)) where
  foldingWithIndex _ _ acc _ = acc

class MaybeToComponentReference (isComponentReference :: Boolean) from to | isComponentReference from -> to where
  maybeToComponentReference :: (BProxy isComponentReference) -> from -> ReversedSegments -> to

instance maybeToComponentReferenceFalse :: MaybeToComponentReference False from NotAType where
  maybeToComponentReference _ _ _ = NotAType

instance maybeToComponentReferenceTrue ::
  ToComponentReference from =>
  MaybeToComponentReference True from ComponentReference where
  maybeToComponentReference _ = toComponentReference

class ToComponentReferences from where
  toComponentReferences :: from -> ReversedSegments -> Object ComponentReference

instance toComponentReferencesR ::
  HFoldlWithIndex
      ToComponentReferencesFolding
      (List (Tuple String ComponentReference))
      (Record from)
      (List (Tuple String ComponentReference)) =>
  ToComponentReferences (Record from) where
  toComponentReferences from segments = Object.fromFoldable acc
    where
    acc = hfoldlWithIndex (ToComponentReferencesFolding segments) (Nil :: List (Tuple String ComponentReference)) from

instance toComponentReferencesSR ::
  ( HFoldlWithIndex
        ToComponentReferencesFolding
        (List (Tuple String ComponentReference))
        (SubRecord from)
        (List (Tuple String ComponentReference))
    ) =>
  ToComponentReferences (SubRecord from) where
  toComponentReferences from segments = Object.fromFoldable acc
    where
    acc = hfoldlWithIndex (ToComponentReferencesFolding segments) (Nil :: List (Tuple String ComponentReference)) from

data ToComponentReferencesFolding
  = ToComponentReferencesFolding ReversedSegments

instance toComponentReferencesStates ::
  FoldingWithIndex
    ToComponentReferencesFolding
    (SProxy "states")
    (List (Tuple String ComponentReference))
    v
    (List (Tuple String ComponentReference)) where
  foldingWithIndex _ _ acc _ = acc
else instance toComponentReferencesNullable ::
  FoldingWithIndex
    ToComponentReferencesFolding
    (SProxy prop)
    (List (Tuple String ComponentReference))
    (Nullable v)
    (List (Tuple String ComponentReference)) where
  foldingWithIndex f prop acc nv = case toMaybe nv of
    Nothing -> acc
    Just v -> foldingWithIndex f prop acc v
else instance toComponentReferencesFoldingR ::
  ( IsSymbol prop
  , IsBoolean isComponentReference
  , IsBoolean hasComponent
  , IsBoolean hasStates
  , HasPropAny "states" v hasStates
  , HasProp "component" String v hasComponent
  , IsComponentReference v isComponentReference
  , MaybeToComponentReference isComponentReference (Record v) v'
  , MaybeFrom v' ComponentReference (Maybe ComponentReference)
  ) =>
  FoldingWithIndex
    ToComponentReferencesFolding
    (SProxy prop)
    (List (Tuple String ComponentReference))
    (Record v)
    (List (Tuple String ComponentReference)) where
  foldingWithIndex (ToComponentReferencesFolding segments) prop acc v = case v'' of
    Nothing -> acc
    Just value -> Tuple k value : acc
    where
    k = reflectSymbol prop

    v' = maybeToComponentReference (BProxy :: BProxy isComponentReference) v (addSegment k segments)

    v'' = maybeFrom Nothing Just v'
else instance toComponentReferencesFoldingSR ::
  ( IsSymbol prop
  , IsComponentReference v isComponentReference
  , MaybeToComponentReference isComponentReference (SubRecord v) v'
  , MaybeFrom v' ComponentReference (Maybe ComponentReference)
  ) =>
  FoldingWithIndex
    ToComponentReferencesFolding
    (SProxy prop)
    (List (Tuple String ComponentReference))
    (SubRecord v)
    (List (Tuple String ComponentReference)) where
  foldingWithIndex (ToComponentReferencesFolding segments) prop acc v = case v'' of
    Nothing -> acc
    Just value -> Tuple k value : acc
    where
    k = reflectSymbol prop

    v' = maybeToComponentReference (BProxy :: BProxy isComponentReference) v (addSegment k segments)

    v'' = maybeFrom Nothing Just v'
else instance toComponentReferencesFoldingA ::
  FoldingWithIndex
    ToComponentReferencesFolding
    (SProxy prop)
    (List (Tuple String ComponentReference))
    v
    (List (Tuple String ComponentReference)) where
  foldingWithIndex _ _ acc _ = acc

------------------------------ unconditional conversions ------------------------------
toValue :: forall a. Reify a => a -> Value
toValue = mkValue

class ToPropsTree from where
  toPropsTree :: from -> PropsTree

instance toPropsTreeR ::
  ToGroup (Record from) =>
  ToPropsTree (Record from) where
  toPropsTree = Group <<< toGroup
else instance toPropsTreeSR ::
  ToGroup (SubRecord from) =>
  ToPropsTree (SubRecord from) where
  toPropsTree = Group <<< toGroup
else instance toPropsTreeA ::
  Reify from =>
  ToPropsTree from where
  toPropsTree = Prop <<< toValue

class ToGroup from where
  toGroup :: from -> Group

instance toGroupR ::
  HFoldlWithIndex
      ToGroupFolding
      (List (Tuple String PropsTree))
      (Record from)
      (List (Tuple String PropsTree)) =>
  ToGroup (Record from) where
  toGroup from = Object.fromFoldable acc
    where
    acc = hfoldlWithIndex ToGroupFolding (Nil :: List (Tuple String PropsTree)) from

instance toGroupSR ::
  HFoldlWithIndex
      ToGroupFolding
      (List (Tuple String PropsTree))
      (SubRecord from)
      (List (Tuple String PropsTree)) =>
  ToGroup (SubRecord from) where
  toGroup from = Object.fromFoldable acc
    where
    acc = hfoldlWithIndex ToGroupFolding (Nil :: List (Tuple String PropsTree)) from

data ToGroupFolding
  = ToGroupFolding

instance toGroupFoldingNullable ::
  FoldingWithIndex
      ToGroupFolding
      (SProxy prop)
      (List (Tuple String PropsTree))
      from
      (List (Tuple String PropsTree)) =>
  FoldingWithIndex
    ToGroupFolding
    (SProxy prop)
    (List (Tuple String PropsTree))
    (Nullable from)
    (List (Tuple String PropsTree)) where
  foldingWithIndex f prop acc nfrom = case toMaybe nfrom of
    Nothing -> acc
    Just from -> foldingWithIndex f prop acc from
else instance toGroupFolding ::
  ( IsSymbol prop
  , ToPropsTree from
  ) =>
  FoldingWithIndex
    ToGroupFolding
    (SProxy prop)
    (List (Tuple String PropsTree))
    from
    (List (Tuple String PropsTree)) where
  foldingWithIndex _ prop acc from = Tuple k v : acc
    where
    k = reflectSymbol prop

    v = toPropsTree from

class ToSection from where
  toSection :: from -> ReversedSegments -> Section

instance toSectionR ::
  HFoldlWithIndex
      ToSectionFolding
      (List (Tuple String ConfigTree))
      (Record from)
      (List (Tuple String ConfigTree)) =>
  ToSection (Record from) where
  toSection from segments = Object.fromFoldable acc
    where
    acc = hfoldlWithIndex (ToSectionFolding segments) (Nil :: List (Tuple String ConfigTree)) from

instance toSectionSR ::
  HFoldlWithIndex
      ToSectionFolding
      (List (Tuple String ConfigTree))
      (SubRecord from)
      (List (Tuple String ConfigTree)) =>
  ToSection (SubRecord from) where
  toSection from segments = Object.fromFoldable acc
    where
    acc = hfoldlWithIndex (ToSectionFolding segments) (Nil :: List (Tuple String ConfigTree)) from

data ToSectionFolding
  = ToSectionFolding ReversedSegments

instance toSectionFoldingNullable ::
  FoldingWithIndex
      ToSectionFolding
      (SProxy prop)
      (List (Tuple String ConfigTree))
      from
      (List (Tuple String ConfigTree)) =>
  FoldingWithIndex
    ToSectionFolding
    (SProxy prop)
    (List (Tuple String ConfigTree))
    (Nullable from)
    (List (Tuple String ConfigTree)) where
  foldingWithIndex f prop acc nfrom = case toMaybe nfrom of
    Nothing -> acc
    Just from -> foldingWithIndex f prop acc from
else instance toSectionFolding ::
  ( IsSymbol prop
  , ToConfigTree from
  ) =>
  FoldingWithIndex
    ToSectionFolding
    (SProxy prop)
    (List (Tuple String ConfigTree))
    from
    (List (Tuple String ConfigTree)) where
  foldingWithIndex (ToSectionFolding segments) prop acc from = Tuple k v : acc
    where
    k = reflectSymbol prop

    v = toConfigTree from (addSegment k segments)

class ToStateProps from where
  toStateProps :: from -> StateProps

instance toStatePropsR ::
  ( ToProps (Record from)
  , ToSubStates (Record from)
  ) =>
  ToStateProps (Record from) where
  toStateProps from = StateProps { props, subComponents }
    where
    props = toProps from

    subComponents = toSubStates from

instance toStatePropsSR ::
  ( ToProps (SubRecord from)
  , ToSubStates (SubRecord from)
  ) =>
  ToStateProps (SubRecord from) where
  toStateProps from = StateProps { props, subComponents }
    where
    props = toProps from

    subComponents = toSubStates from

class ToSubState from where
  toSubState :: from -> SubState

instance toSubStateR ::
  ( ToProps (Record from)
  , ToSubStates (Record from)
  , IsSymbol statesProp
  , Append "states" "" statesProp -- can't use "states" directly for some reason, have to introduce intermediate type
  , Row.Cons statesProp states trash from
  , ToStates states
  ) =>
  ToSubState (Record from) where
  toSubState from = SubState { props, subComponents, states }
    where
    props = toProps from

    subComponents = toSubStates from

    states = toStates (Record.get (SProxy :: SProxy statesProp) from)

instance toSubStateSR ::
  ( ToProps (SubRecord from)
  , ToSubStates (SubRecord from)
  , IsSymbol statesProp
  , Append "states" "" statesProp -- can't use "states" directly for some reason, have to introduce intermediate type
  , Row.Cons statesProp states trash from
  , ToStates states
  ) =>
  ToSubState (SubRecord from) where
  toSubState from = SubState { props, subComponents, states }
    where
    props = toProps from

    subComponents = toSubStates from

    states = maybe Object.empty toStates (toMaybe $ SubRecord.get (SProxy :: SProxy statesProp) from)

class ToStates from where
  toStates :: from -> States

instance toStatesNullable ::
  ToStates (SubRecord from) =>
  ToStates (Nullable (SubRecord from)) where
  toStates nfrom = toStates from
    where
    from = fromMaybe (mkSubRecord {} :: SubRecord from) (toMaybe nfrom)

instance toStatesR ::
  ( HMap ToStatePropsMapping (Record from) (Record to)
  , Homogeneous to StateProps
  ) =>
  ToStates (Record from) where
  toStates from = Object.fromHomogeneous to
    where
    to :: Record to
    to = hmap ToStatePropsMapping from

instance toStatesSR ::
  ( HMap (SRtoR ToStatePropsMapping) (SubRecord from) (Record to)
  , Homogeneous to (Maybe StateProps)
  ) =>
  ToStates (SubRecord from) where
  toStates from = Object.fromFoldable result
    where
    to :: Record to
    to = hmap (SRtoR ToStatePropsMapping) from

    flat :: Array (Tuple String (Maybe StateProps))
    flat = Object.toUnfoldable $ Object.fromHomogeneous to

    result = mapMaybe sequence flat

data ToStatePropsMapping
  = ToStatePropsMapping

instance toStatePropsMappingNullable ::
  ToStateProps from =>
  Mapping
    ToStatePropsMapping
    (Nullable from)
    (Maybe StateProps) where
  mapping _ nfrom = toStateProps <$> toMaybe nfrom
else instance toStatePropsMapping ::
  ToStateProps from =>
  Mapping
    ToStatePropsMapping
    from
    StateProps where
  mapping _ = toStateProps

class ToComponentReference from where
  toComponentReference :: from -> ReversedSegments -> ComponentReference

instance toComponentReferenceR ::
  ( ToProps (Record from)
  , ToComponentReferences (Record from)
  , IsSymbol statesProp
  , Append "states" "" statesProp -- can't use "states" directly for some reason, have to introduce intermediate type
  , Row.Cons statesProp states trash from
  , IsSymbol componentProp
  , Append "component" "" componentProp -- can't use "states" directly for some reason, have to introduce intermediate type
  , Row.Cons componentProp String trash2 from
  , ToStates states
  ) =>
  ToComponentReference (Record from) where
  toComponentReference from segments =
    ComponentReference
      { component
      , props
      , states
      , subComponents
      }
    where
    props = toProps from

    states = toStates (Record.get (SProxy :: SProxy statesProp) from)

    subComponents = toComponentReferences from segments

    reference = Just $ Record.get (SProxy :: SProxy componentProp) from

    component =
      { identifier: getIdentifierByPath segments
      , reference
      }

instance toComponentReferenceSR ::
  ( ToProps (SubRecord from)
  , ToComponentReferences (SubRecord from)
  , IsSymbol statesProp
  , Append "states" "" statesProp -- can't use "states" directly for some reason, have to introduce intermediate type
  , Row.Cons statesProp states trash from
  , IsSymbol componentProp
  , Append "component" "" componentProp -- can't use "states" directly for some reason, have to introduce intermediate type
  , Row.Cons componentProp String trash2 from
  , ToStates states
  ) =>
  ToComponentReference (SubRecord from) where
  toComponentReference from segments =
    ComponentReference
      { component
      , props
      , states
      , subComponents
      }
    where
    props = toProps from

    states = maybe Object.empty toStates (toMaybe $ SubRecord.get (SProxy :: SProxy statesProp) from)

    subComponents = toComponentReferences from segments

    reference = toMaybe $ SubRecord.get (SProxy :: SProxy componentProp) from

    component =
      { identifier: getIdentifierByPath segments
      , reference
      }

class ToComponentDefinition from where
  toComponentDefinition :: from -> ReversedSegments -> ComponentDefinition

instance toComponentDefinitionR ::
  ( ToProps (Record from)
  , ToComponentReferences (Record from)
  , Append "states" "" statesProp -- can't use "states" directly for some reason, have to introduce intermediate type
  , IsSymbol statesProp
  , Row.Cons statesProp states trash from
  , ToStates states
  ) =>
  ToComponentDefinition (Record from) where
  toComponentDefinition from segments =
    ComponentDefinition
      { identifier
      , props
      , states
      , subComponents
      }
    where
    props = toProps from

    states = toStates (Record.get (SProxy :: SProxy statesProp) from)

    -- states = Object.empty
    subComponents = toComponentReferences from segments

    identifier = getIdentifierByPath segments

instance toComponentDefinitionSR ::
  ( ToProps (SubRecord from)
  , ToComponentReferences (SubRecord from)
  , Append "states" "" statesProp -- can't use "states" directly for some reason, have to introduce intermediate type
  , IsSymbol statesProp
  , Row.Cons statesProp states trash from
  , ToStates states
  ) =>
  ToComponentDefinition (SubRecord from) where
  toComponentDefinition from segments =
    ComponentDefinition
      { identifier
      , props
      , states
      , subComponents
      }
    where
    props = toProps from

    states = maybe Object.empty toStates (toMaybe $ SubRecord.get (SProxy :: SProxy statesProp) from)

    subComponents = toComponentReferences from segments

    identifier = getIdentifierByPath segments
