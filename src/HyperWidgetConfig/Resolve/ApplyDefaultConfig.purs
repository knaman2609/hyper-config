module HyperWidgetConfig.Resolve.ApplyDefaultConfig where

import Data.SubRecord (SubRecord)
import Data.SubRecord.Heterogeneous (class HFillFromBiggerRecursive, hfillFromBiggerRecursive)
import HyperWidgetConfig.Config.RequireProps (class RequireProps_)

class ApplyDefaultConfig_ props required where
  applyDefaultConfig' :: required -> props -> required

instance applyDefaultConfigInst ::
  ( HFillFromBiggerRecursive (Record required) (SubRecord props) (Record required)
  , RequireProps_ (SubRecord props) (Record required)
  ) =>
  ApplyDefaultConfig_ (SubRecord props) (Record required) where
  applyDefaultConfig' config optionalConfig = hfillFromBiggerRecursive config optionalConfig
