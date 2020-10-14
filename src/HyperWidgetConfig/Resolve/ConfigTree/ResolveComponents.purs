module HyperWidgetConfig.Resolve.ConfigTree.ResolveComponents
  ( ResolvedComponentsConfigRoot
  , F
  , runF
  , ResolveStatus
  , toConfigRoot
  , resolveComponents
  , resolveComponentDefinition
  ) where

import Prelude
import Control.Alt ((<|>))
import Control.Apply (lift2)
import Control.Comonad (extract)
import Control.Comonad.Env (Env, ask, env, local)
import Control.Monad.Except (Except, ExceptT, mapExceptT, runExceptT, throwError)
import Control.Monad.ST (ST)
import Control.Monad.ST as ST
import Control.Monad.State (StateT, evalStateT, get, put)
import Control.Monad.Trans.Class (lift)
import Data.Array (mapMaybe, singleton, union)
import Data.Bifunctor (lmap)
import Data.Either (either)
import Data.Lens (Lens, Lens', itraverseOf, set, (%~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (SProxy(..))
import Data.Traversable (sequence)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))
import Data.Validation.Semigroup (V, invalid, unV)
import Extra.Data.Lens.Indexed (ReversedSegments, addSegment, fromPath, toPath)
import Foreign.Object (Object)
import Foreign.Object as Object
import Foreign.Object.ST (STObject, delete, new, peek, poke)
import HyperWidgetConfig.Error (ConfigError, ResolveError(..), packError)
import HyperWidgetConfig.Resolve.ConfigTree.Probe (probeComponentDefinition)
import HyperWidgetConfig.Resolve.ConfigTree.Types (ComponentDefinition(..), ComponentReference(..), ComponentReferenceValue, ConfigRoot, PropsTree(..), StateProps(..), SubState(..), _componentDefinitions, _identifier)

newtype ResolvedComponentsConfigRoot
  = ResolvedComponentsConfigRoot ConfigRoot

toConfigRoot :: ResolvedComponentsConfigRoot -> ConfigRoot
toConfigRoot (ResolvedComponentsConfigRoot cr) = cr

-- | `ExceptT ConfigError` - to allow to exit early if error is too severe to continue resolving this particular component.
-- | `StateT (STObject r ResolveStatus) (ST r)` - to keep state of object map with resolved statuses,
-- | each key corresponds to the tried path, and values correspond to resolve status. This is to prevent
-- | circular reference dead lock.
-- | Note: Not exactly sure, but I think it is not possible to have dead locks with current implementation.
-- | We do not allow overrides to reference another override, that means we will get a container identifier mismatch
-- | _before_ we dive into recursive overrides propagation.
type M' r a
  = ExceptT ConfigError (StateT (STObject r ResolveStatus) (ST r)) a

-- | It is not actually a monad because `V` is not a monad.
-- | It's main purpose is to have both of these:
-- | - "critial" errors which allow to abort early,
-- | - "warning" errors which allow to continue with applicative validation.
type F' r a
  = M' r (V (Array ConfigError) a)

type M r a
  = StateT (STObject r ResolveStatus) (ST r) a

-- | It is not actually a monad because `V` is not a monad.
newtype F r a
  = F (M r (V (Array ConfigError) a))

derive instance newtypeF :: Newtype (F r a) _

derive instance functorF :: Functor (F r)

instance applyF :: Apply (F r) where
  apply (F fvf) (F fva) =
    F do
      -- WARN:
      -- Not 100% sure, but it might not be a law-ful applicative definition.
      -- Applicative apply must not care about the order of execution,
      -- and here we do monadic actions which manipulate state, so they definitely
      -- care about order of execution if executing in parallel. For sequencial
      -- execution it doesn't matter.
      vf <- fvf
      va <- fva
      pure $ apply vf va

instance applicativeF :: Applicative (F r) where
  pure = F <<< pure <<< pure

data ResolveStatus
  = Started

type Environment
  = { config :: ConfigRoot, path :: ReversedSegments }

_config :: Lens' Environment ConfigRoot
_config = prop (SProxy :: SProxy "config")

_path :: Lens' Environment ReversedSegments
_path = prop (SProxy :: SProxy "path")

addSegmentE :: String -> Environment -> Environment
addSegmentE segment = _path %~ addSegment segment

runF :: forall a. (forall r. F r a) -> (V (Array ConfigError) a)
runF (F f) = ST.run (new >>= evalStateT f)

mapF' :: forall r a b. (a -> b) -> F' r a -> F' r b
mapF' = map <<< map

-- | Like `over`, but useful when mapping function produces F' effect.
overF' :: forall s t a b r. Lens s t a b -> (a → F' r b) -> s -> F' r t
overF' _l f s = mapF' (\b -> set _l b s) fb
  where
  a = s ^. _l

  fb = f a

pureF' :: forall r a. a -> F' r a
pureF' = pure <<< pure

composeKleisliF' :: forall r a b c. (a -> F' r b) -> (b -> F' r c) -> (a -> F' r c)
composeKleisliF' f g = f >=> unV (pure <<< invalid) g

-- | `F'` is fine in the context of resolving singular component, but in the end we do not want
-- | to abort the entire resolve process just because there is an error for some component.
-- | So we get rid of ExceptT and put it's error into applicative validation so we can continue on resolving.
coerceF' :: forall r a. F' r a -> F r a
coerceF' = F <<< map (either (invalid <<< singleton) identity) <<< runExceptT

liftST' :: forall r a. ST r a -> M' r a
liftST' = lift <<< lift

liftM' :: forall r a. M' r a -> F' r a
liftM' = map pure

liftExceptM' :: forall r a. { path :: String, reference :: String } -> Except ResolveError a -> M' r a
liftExceptM' info = mapExceptT (pure <<< lmap (flip packError info) <<< unwrap)

liftExceptF :: forall r a. { path :: String, reference :: String } -> Except ResolveError a -> F r a
liftExceptF info = coerceF' <<< liftM' <<< liftExceptM' info

modifyM' :: forall r. (STObject r ResolveStatus -> M' r (STObject r ResolveStatus)) -> M' r Unit
modifyM' f = get >>= f >>= put

started :: forall r. String -> M' r Unit
started path = throwIfAlreadyStarted *> modifyM' (liftST' <<< poke path Started)
  where
  throwIfAlreadyStarted =
    get
      >>= (liftST' <<< peek path)
      >>= case _ of
          Just Started ->
            throwError
              $ { path
                , reference: "" -- TODO: doesn't look good
                , error: CircularReference
                }
          Nothing -> pure unit

resolved :: forall r. String -> M' r Unit
resolved path = modifyM' (liftST' <<< delete path)

resolveComponents :: ConfigRoot -> V (Array ConfigError) ResolvedComponentsConfigRoot
resolveComponents config = ResolvedComponentsConfigRoot <$> runF (itraverseOf _componentDefinitions f config)
  where
  f :: forall r. ReversedSegments -> ComponentDefinition -> F r ComponentDefinition
  f path cd = resolveComponentDefinition $ env { config, path } cd

-- | Here we assume self props of ComponentDefinition are already filled,
-- | and we only need to deal with subComponents.
resolveComponentDefinition ::
  forall r.
  Env Environment ComponentDefinition -> -- TODO: why do I even need Env. Just pass two arguments instead?
  F r ComponentDefinition
resolveComponentDefinition e =
  let
    cd = extract e

    path = toPath (ask e ^. _path)

    _subComponents :: Lens' ComponentDefinition (Object ComponentReference)
    _subComponents = _Newtype <<< prop (SProxy :: SProxy "subComponents")

    resolveSubComponents :: ComponentDefinition -> F' r ComponentDefinition
    resolveSubComponents =
      overF' _subComponents
        ( map sequence
            <<< traverseWithIndex
                ( \i cr ->
                    let
                      e' = cr <$ local (addSegmentE i) e
                    in
                      lift $ unwrap $ resolveComponentReference e'
                )
        )

    pipeline = resolveSubComponents
  in
    coerceF'
      ( started path *> pipeline cd <* resolved path
      )

resolveComponentReference ::
  forall r.
  Env Environment ComponentReference ->
  F r ComponentReference
resolveComponentReference e =
  let
    cr = extract e

    path = toPath (ask e ^. _path)

    _subComponents :: Lens' ComponentReference (Object ComponentReference)
    _subComponents = _Newtype <<< prop (SProxy :: SProxy "subComponents")

    _component :: Lens' ComponentReference ComponentReferenceValue
    _component = _Newtype <<< prop (SProxy :: SProxy "component")

    resolveSelfAndOverrides :: ComponentReference -> F' r ComponentReference
    resolveSelfAndOverrides cr' =
      liftM' do
        let
          referenceValue = cr' ^. _component

          config = ask e ^. _config
        reference <- case referenceValue.reference of
          Nothing ->
            -- Should not be reachable
            throwError
              $ packError MissingReference
                  { path
                  , reference: ""
                  }
          Just v -> pure v
        let
          info =
            { path
            , reference
            }
        cd <- liftExceptM' info $ probeComponentDefinition (fromPath reference) config
        when (cd ^. _identifier /= referenceValue.identifier)
          let
            err =
              packError
                ( WrongComponentReferenceType
                    { expected: referenceValue.identifier
                    , actual: cd ^. _identifier
                    }
                )
                info
          in
            throwError err
        pure $ applyComponentDefinition cd cr'

    resolveSubComponents :: ComponentReference -> F' r ComponentReference
    resolveSubComponents =
      overF' _subComponents
        ( map sequence
            <<< traverseWithIndex
                ( \i cr' ->
                    let
                      e' = cr' <$ local (addSegmentE i) e
                    in
                      lift $ unwrap $ resolveComponentReference e'
                )
        )

    pipeline = resolveSelfAndOverrides `composeKleisliF'` resolveSubComponents
  in
    coerceF'
      ( started path *> pipeline cr <* resolved path
      )

applyObject' :: forall a b c. (Maybe a -> Maybe b -> Maybe c) -> Object a -> Object b -> Object c
applyObject' fnMaybe o1 o2 = Object.fromFoldable $ mapMaybe applyKey keys
  where
  keys1 = Object.keys o1

  keys2 = Object.keys o2

  keys = union keys1 keys2

  applyKey k = Tuple k <$> fnMaybe v1 v2
    where
    v1 = Object.lookup k o1

    v2 = Object.lookup k o2

applyObject :: forall a. (a -> a -> a) -> Object a -> Object a -> Object a
applyObject fn = applyObject' fnMaybe
  where
  fnMaybe l r = lift2 fn l r <|> r <|> l

applyComponentDefinition :: ComponentDefinition -> ComponentReference -> ComponentReference
applyComponentDefinition (ComponentDefinition cd) (ComponentReference cr) =
  ComponentReference
    { component: cr.component
    , props: applyObject applyPropsTree cd.props cr.props
    , states: applyObject applyStateProps cd.states cr.states
    , subComponents: applyObject applyComponentReference cd.subComponents cr.subComponents
    }

applyComponentReference :: ComponentReference -> ComponentReference -> ComponentReference
applyComponentReference (ComponentReference cr1) (ComponentReference cr2) =
  ComponentReference
    { component:
        { identifier: cr2.component.identifier
        , reference: cr2.component.reference <|> cr1.component.reference
        }
    , props: applyObject applyPropsTree cr1.props cr2.props
    , states: applyObject applyStateProps cr1.states cr2.states
    , subComponents: applyObject applyComponentReference cr1.subComponents cr2.subComponents
    }

applyStateProps :: StateProps -> StateProps -> StateProps
applyStateProps (StateProps sp1) (StateProps sp2) =
  StateProps
    { props: applyObject applyPropsTree sp1.props sp2.props
    , subComponents: applyObject applySubState sp1.subComponents sp2.subComponents
    }

applySubState :: SubState -> SubState -> SubState
applySubState (SubState ss1) (SubState ss2) =
  SubState
    { props: applyObject applyPropsTree ss1.props ss2.props
    , states: applyObject applyStateProps ss1.states ss2.states
    , subComponents: applyObject applySubState ss1.subComponents ss2.subComponents
    }

applyPropsTree :: PropsTree -> PropsTree -> PropsTree
applyPropsTree l r = case l, r of
  Group gl, Group gr -> Group $ applyObject applyPropsTree gl gr
  _, pr -> pr
