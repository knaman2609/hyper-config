module HyperWidgetConfig.Resolve.ForAllAttachPropertyInfo where

import Prelude
import Control.Monad.Except (Except, runExcept)
import Data.Array (singleton)
import Data.Either (either)
import Data.Validation.Semigroup (V, invalid)
import Extra.Data.Lens.Indexed (ReversedSegments, toPath)
import Extra.Heterogeneous (MappingWithIndex, mkRecursiveMappingWithIndexAggregated)
import Extra.Heterogeneous.ForAll (ForAllMappingN(..))
import Extra.Heterogeneous.Loosely (LooselyMapping(..))
import Extra.Heterogeneous.Recursive (RecursiveMappingWithIndex)
import Heterogeneous.Mapping (class HMapWithIndex, hmapWithIndex)
import HyperWidgetConfig.Error (ReferenceErrorEnvelope, ConfigError, attachPropertyInfo)

class ForAllAttachPropertyInfo from to | from -> to where
  forAllAttachPropertyInfo :: from -> to

instance forAllAttachPropertyInfoSS ::
  HMapWithIndex
      ( RecursiveMappingWithIndex
          ( MappingWithIndex
              ( LooselyMapping
                  ( ForAllMappingN
                      (Except ReferenceErrorEnvelope)
                      (V (Array ConfigError))
                  )
              )
          )
      )
      (Record from)
      (Record to) =>
  ForAllAttachPropertyInfo (Record from) (Record to) where
  forAllAttachPropertyInfo = hmapWithIndex m
    where
    m = mkRecursiveMappingWithIndexAggregated mkF

    mkF :: ReversedSegments -> LooselyMapping (ForAllMappingN (Except ReferenceErrorEnvelope) (V (Array ConfigError)))
    mkF segments = LooselyMapping $ ForAllMappingN fn
      where
      fn :: forall a. Except ReferenceErrorEnvelope a -> V (Array ConfigError) a
      fn = either mkError pure <<< runExcept
        where
        mkError :: ReferenceErrorEnvelope -> V (Array ConfigError) a
        mkError = invalid <<< singleton <<< attachPropertyInfo (toPath segments)
