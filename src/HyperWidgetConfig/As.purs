module HyperWidgetConfig.As where

import Data.SubRecord (SubRecord)

type StatePropagatedProps props
  = Record props

type Props props
  = SubRecord props

type RequiredProps props
  = Record props

type ResolvedComponentsProps props
  = Record props

type PropagatedStatesProps props
  = Record props

type ResolvedProps props
  = Record props

type ReferencePropsRow props
  = ( component :: String
    | props
    )

type ReferenceProps props
  = SubRecord (ReferencePropsRow props)

type ReferenceRequiredProps props
  = Record (ReferencePropsRow props)

type ReferenceResolvedComponentsProps props
  = Record (ReferencePropsRow props)

type ReferencePropagatedStatesProps props
  = Record (ReferencePropsRow props)

type ReferenceResolvedProps props
  = Record (ReferencePropsRow props)
