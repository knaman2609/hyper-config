module HyperWidgetConfig.Resolve
  ( ResolveValue
  , resolveValue
  , resolveConfig
  , applyDefaultConfig
  , resolveStates
  , resolveComponents
  , resolveValues
  , module ForAllAttachPropertyInfo
  ) where

import Prelude
import Control.Monad.Except (Except, throwError)
import Data.Array as Array
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), joinWith)
import Data.Validation.Semigroup (V, andThen)
import Extra.Data.String (split1)
import Foreign.Object (Object)
import Foreign.Object as Object
import HyperWidgetConfig.Config as Config
import HyperWidgetConfig.Error (ReferenceErrorEnvelope, ResolveError(..), ConfigError, packReferenceError)
import HyperWidgetConfig.Reference (Reference(..))
import HyperWidgetConfig.Reify (class Reify, RuntimeType(..), reify)
import HyperWidgetConfig.Resolve.ApplyDefaultConfig (applyDefaultConfig')
import HyperWidgetConfig.Resolve.ConfigTree.ResolveComponents as ConfigTree
import HyperWidgetConfig.Resolve.ConfigTree.Types (ConfigRoot(..), ConfigTree(..))
import HyperWidgetConfig.Resolve.ForAllAttachPropertyInfo (class ForAllAttachPropertyInfo, forAllAttachPropertyInfo) as ForAllAttachPropertyInfo
import HyperWidgetConfig.Resolve.FromConfigTree (fromResolvedComponentsConfigRoot)
import HyperWidgetConfig.Resolve.ResolveStates (resolveStates')
import HyperWidgetConfig.Resolve.ResolveValues (resolveValues')
import HyperWidgetConfig.Resolve.ToConfigTree (toConfigTree)
import Partial.Unsafe (unsafeCrashWith)
import Record.Unsafe (unsafeGet)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

toConfigRoot :: Config.RequiredProps -> ConfigRoot
toConfigRoot config = ConfigRoot (toSection (toConfigTree config mempty))
  where
  toSection (Section section) = section

  toSection _ = unsafeCrashWith "unreachable"

applyDefaultConfig :: Config.RequiredProps -> Config.Props -> Config.RequiredProps
applyDefaultConfig config optionalConfig = applyDefaultConfig' config optionalConfig

resolveComponents :: Config.RequiredProps -> V (Array ConfigError) Config.ResolvedComponentsProps
resolveComponents = map fromResolvedComponentsConfigRoot <<< ConfigTree.resolveComponents <<< toConfigRoot

resolveStates :: Config.ResolvedComponentsProps -> Config.PropagatedStatesProps
resolveStates config = resolveStates' config

resolveValues :: Config.PropagatedStatesProps -> V (Array ConfigError) Config.ResolvedProps
resolveValues config = resolveValues' config fn
  where
  fn :: forall a. ResolveValue a
  fn = resolveValue config

resolveConfig :: Config.RequiredProps -> Config.Props -> V (Array ConfigError) Config.ResolvedProps
resolveConfig defaultConfig =
  applyDefaultConfig defaultConfig
    >>> resolveComponents
    >>> map resolveStates
    >>> flip andThen resolveValues

-- TODO: check if these runtimetype shenanigans could be done with Generic instead
validatePathType :: Object RuntimeType -> RuntimeType -> List String -> Maybe ResolveError
validatePathType runtimeTypes runtimeType segments = fn runtimeTypes segments Nil
  where
  fn _ Nil _ = Nothing

  fn acc (s : Nil) _ = case Object.lookup s acc of
    Nothing -> Just NotFound
    Just rt ->
      if rt == runtimeType then
        Nothing
      else
        Just
          $ WrongType
              { expected: runtimeType
              , actual: rt
              }

  fn acc (s : ss) pathSoFarReversed = case Object.lookup s acc of
    Just (RuntimeRecord acc') -> fn acc' ss pathSoFarReversed'
    Just _ -> Just $ Conflict { conflictAt: (joinWith "." <<< Array.reverse <<< Array.fromFoldable) pathSoFarReversed' }
    Nothing -> Just NotFound
    where
    pathSoFarReversed' = s : pathSoFarReversed

type ResolveValue a
  = Reify a => Reference a -> Except ReferenceErrorEnvelope a

resolveValue :: forall a. Config.PropagatedStatesProps -> ResolveValue a
resolveValue config = case _ of
  Literal a -> pure a
  Reference reference ->
    let
      segments = List.fromFoldable $ split1 (Pattern ".") reference

      runtimeType = reify (Proxy :: Proxy a)

      unsafeRecordFocus Nil acc = acc

      unsafeRecordFocus (s : ss) acc = unsafeRecordFocus ss acc'
        where
        -- Record.unsafeGet is a little bit too unsafe (with runtime error throwing if not found)
        -- but since we have a check of runtimeTypes it should be safe to use.
        acc' = unsafeGet s acc
    in
      case validatePathType Config.propagatedStatesRuntimeTypes runtimeType segments of
        -- we passed runtime types check, so if actually found something, it's gonna be what we want
        -- so unsafeCoerce is fine
        Nothing -> pure $ unsafeCoerce $ unsafeRecordFocus segments config
        Just error -> throwError $ packReferenceError error reference
