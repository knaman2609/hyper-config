module HyperWidgetConfig.Resolve.ConfigTree.Probe where

import Prelude
import Control.Alt ((<|>))
import Control.Monad.Except (Except, throwError)
import Data.Either (Either)
import Data.Foldable (foldM)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Prism', prism', (^?))
import Data.List (List(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Extra.Data.Lens.Indexed (ReversedSegments(..), Segments(..), addSegment, toPath)
import Extra.Data.Lens.Prism (eitherPrisms)
import Foreign.Object as Object
import HyperWidgetConfig.Error (ResolveError(..))
import HyperWidgetConfig.Resolve.ConfigTree.Types (ComponentDefinition(..), ComponentReference(..), ConfigRoot(..), ConfigTree(..), Group, PropsTree(..), Section, StateProps(..), States, SubState(..), ComponentReferenceValue)
import HyperWidgetConfig.Resolve.ConfigTree.Value (Value)

data Probe
  = PSection Section
  | PComponentDefinition ComponentDefinition
  | PValue Value
  | PRoot Section
  | PComponentReference ComponentReference
  | PStates States
  | PStateProps StateProps
  | PSubState SubState
  | PComponentReferenceValue ComponentReferenceValue
  | PPropsGroup Group
  | PPropsValue Value

derive instance eqProbe :: Eq Probe

derive instance genericProbe :: Generic Probe _

instance showProbe :: Show Probe where
  show = genericShow

_pcomponentDefinition :: Prism' Probe ComponentDefinition
_pcomponentDefinition =
  prism' PComponentDefinition case _ of
    PComponentDefinition cd -> Just cd
    _ -> Nothing

_pcomponentReference :: Prism' Probe ComponentReference
_pcomponentReference =
  prism' PComponentReference case _ of
    PComponentReference cr -> Just cr
    _ -> Nothing

_pcomponent :: Prism' Probe (Either ComponentDefinition ComponentReference)
_pcomponent = eitherPrisms _pcomponentDefinition _pcomponentReference

probeComponentDefinition :: Segments -> ConfigRoot -> Except ResolveError ComponentDefinition
probeComponentDefinition = probePrism _pcomponentDefinition

probeComponent :: Segments -> ConfigRoot -> Except ResolveError (Either ComponentDefinition ComponentReference)
probeComponent = probePrism _pcomponent

probePrism :: forall a. Prism' Probe a -> Segments -> ConfigRoot -> Except ResolveError a
probePrism _p segments config = do
  result <- probe_ segments config
  maybe (throwError WrongSection) pure $ result ^? _p

probe_ :: Segments -> ConfigRoot -> Except ResolveError Probe
probe_ segments (ConfigRoot section) = probe segments (PSection section)

probe :: Segments -> Probe -> Except ResolveError Probe
probe (Segments Nil) p = pure p

probe (Segments ss) v = do
  Tuple _ v' <-
    foldM
      ( \(Tuple rss p) s -> do
          p' <- probe' rss p s
          pure $ Tuple (addSegment s rss) p'
      )
      (Tuple (ReversedSegments Nil) v)
      ss
  pure v'

probe' :: ReversedSegments -> Probe -> String -> Except ResolveError Probe
probe' segmentsSoFar p s = case p of
  PSection section -> trySection section
  PComponentDefinition cd -> tryComponentDefinition cd
  PValue _ -> throwError $ Conflict { conflictAt: toPath segmentsSoFar }
  PRoot section -> trySection section
  PComponentReference componentReference -> tryComponentReference componentReference
  PStates states -> maybe (throwError NotFound) (pure <<< PStateProps) $ Object.lookup s states
  PStateProps state -> tryState state
  PSubState subState -> trySubState subState
  PComponentReferenceValue _ -> throwError $ Conflict { conflictAt: toPath segmentsSoFar }
  PPropsGroup group -> tryProps group
  PPropsValue _ -> throwError $ Conflict { conflictAt: toPath segmentsSoFar }
  where
  trySection section = maybe (throwError NotFound) fromConfigTree $ Object.lookup s section

  tryComponentDefinition (ComponentDefinition cd) = case s of
    "states" -> pure $ PStates cd.states
    _ -> tryProps cd.props <|> trySubComponentReferences cd.subComponents

  tryComponentReference (ComponentReference cr) = case s of
    "component" -> pure $ PComponentReferenceValue cr.component
    "states" -> pure $ PStates cr.states
    _ -> tryProps cr.props <|> trySubComponentReferences cr.subComponents

  tryProps props = case Object.lookup s props of
    Nothing -> throwError NotFound
    Just (Group group) -> pure $ PPropsGroup group
    Just (Prop value) -> pure $ PPropsValue value

  trySubComponentReferences subComponents = maybe (throwError NotFound) tryComponentReference $ Object.lookup s subComponents

  trySubStates subComponents = maybe (throwError NotFound) trySubState $ Object.lookup s subComponents

  tryState (StateProps state) = tryProps state.props <|> trySubStates state.subComponents

  trySubState (SubState subState) = case s of
    "states" -> pure $ PStates subState.states
    _ -> tryProps subState.props <|> trySubStates subState.subComponents

  fromConfigTree t =
    pure case t of
      Section v -> PSection v
      Component v -> PComponentDefinition v
      Value v -> PValue v
