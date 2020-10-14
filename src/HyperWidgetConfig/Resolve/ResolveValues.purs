module HyperWidgetConfig.Resolve.ResolveValues where

import Prelude
import Control.Monad.Except (Except)
import Data.Validation.Semigroup (V)
import Extra.Debug (class CompileTimeCheck, any)
import Extra.Heterogeneous.Recursive (RecursiveMapping(..))
import Extra.Heterogeneous.Sequence (class HSequence, hsequenceRecursive)
import Heterogeneous.Mapping (class HMap, class Mapping, hmap)
import HyperWidgetConfig.Error (ReferenceErrorEnvelope, ConfigError)
import HyperWidgetConfig.Reference (Reference)
import HyperWidgetConfig.Reify (class Reify)
import HyperWidgetConfig.Resolve.ForAllAttachPropertyInfo (class ForAllAttachPropertyInfo, forAllAttachPropertyInfo)

inferResolvedValues' :: forall from to. CompileTimeCheck => ResolveValues_ from to => from -> to
inferResolvedValues' _ = any

isResolvedValues' :: forall from to. CompileTimeCheck => ResolveValues_ from to => from -> to -> Boolean
isResolvedValues' _ _ = true

class ResolveValues_ from to | from -> to where
  resolveValues' :: from -> (forall a. Reify a => Reference a -> Except ReferenceErrorEnvelope a) -> V (Array ConfigError) to

instance resolveValuesInst ::
  ( HMap
      ( RecursiveMapping
          ( ResolveValuesMapping Reference (Except ReferenceErrorEnvelope)
          )
      )
      (Record from)
      (Record from')
  , ForAllAttachPropertyInfo (Record from') (Record from'')
  , HSequence (V (Array ConfigError)) (Record from'') (Record to)
  ) =>
  ResolveValues_ (Record from) (Record to) where
  resolveValues' from fn = sequenced
    where
    mapped = hmap (RecursiveMapping (ResolveValuesMapping fn)) from

    attached = forAllAttachPropertyInfo mapped

    sequenced = hsequenceRecursive attached

newtype ResolveValuesMapping f g
  = ResolveValuesMapping (forall a. Reify a => f a -> g a)

instance resolveReferencesMapping :: Reify a => Mapping (ResolveValuesMapping f g) (f a) (g a) where
  mapping (ResolveValuesMapping resolveFn) = resolveFn
else instance resolveReferencesMappingElse :: Mapping (ResolveValuesMapping f g) c c where
  mapping _ = identity
