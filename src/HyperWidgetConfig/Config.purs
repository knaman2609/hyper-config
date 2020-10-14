module HyperWidgetConfig.Config
  ( module Props
  , runtimeTypes
  , propagatedStatesRuntimeTypes
  , resolvedRuntimeTypes
  ) where

import Foreign.Object (Object)
import HyperWidgetConfig.Config.Props
  ( class RequireProps
  , isRequiredProps
  , PropsRow
  , Props
  , ResolvedPropsRow
  , ResolvedProps
  , ResolvedComponentsProps
  , RequiredPropsRow
  , RequiredProps
  , PropagatedStatesPropsRow
  , PropagatedStatesProps
  )
  as Props
import HyperWidgetConfig.Reify (RuntimeType, reifyRecord', reifySubRecord')
import Type.Proxy (Proxy(..))

runtimeTypes :: Object RuntimeType
runtimeTypes = reifySubRecord' (Proxy :: Proxy Props.Props)

propagatedStatesRuntimeTypes :: Object RuntimeType
propagatedStatesRuntimeTypes = reifyRecord' (Proxy :: Proxy Props.PropagatedStatesProps)

resolvedRuntimeTypes :: Object RuntimeType
resolvedRuntimeTypes = reifyRecord' (Proxy :: Proxy Props.ResolvedProps)
