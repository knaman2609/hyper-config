module HyperWidgetConfig.Config.Components.PrimaryButton where

import Prelude
import Data.Nullable (Nullable)
import Data.SubRecord (SubRecord)
import Extra.Debug (class CompileTimeCheck, any)
import HyperWidgetConfig.As as As
import HyperWidgetConfig.Config.RequireProps (isRequiredProps')
import HyperWidgetConfig.Reference (Reference)
import HyperWidgetConfig.Resolve.ResolveAll (isResolved')
import HyperWidgetConfig.Resolve.ResolveComponents (isResolvedComponents')
import HyperWidgetConfig.Resolve.ResolveStates (isResolvedStates')
import HyperWidgetConfig.Resolve.ResolveValues (isResolvedValues')
import Type.Row (type (+))

type SelfPropsRow rest
  = ( margin :: Reference Int | rest )

type ResolvedSelfPropsRow rest
  = ( margin :: Int | rest )

type ReferenceRequiredSelfPropsRow rest
  = ( margin :: Nullable (Reference Int) | rest )

type StatesF state f
  = f
      ( enabled :: state
      )

type State
  = SubRecord (SelfPropsRow + ())

type PropagatedState
  = Record (SelfPropsRow + ())

type ResolvedState
  = Record (ResolvedSelfPropsRow + ())

type PropsRow
  = ( states :: StatesF State SubRecord
    | SelfPropsRow
      + ()
    )

type RequiredPropsRow
  = ( states :: StatesF State SubRecord
    | SelfPropsRow + ()
    )

type ResolvedComponentsPropsRow
  = ( states :: StatesF State SubRecord
    | SelfPropsRow + ()
    )

type PropagatedStatesPropsRow
  = ( states :: StatesF PropagatedState Record
    | SelfPropsRow + ()
    )

type ResolvedPropsRow
  = ( states :: StatesF ResolvedState Record
    | ResolvedSelfPropsRow
      + ()
    )

type ReferenceRequiredPropsRow
  = ( states :: Nullable (StatesF State SubRecord)
    | ReferenceRequiredSelfPropsRow + ()
    )

type ReferenceResolvedComponentsPropsRow
  = ( states :: StatesF State SubRecord
    | SelfPropsRow + ()
    )

type ReferencePropagatedStatesPropsRow
  = ( states :: StatesF PropagatedState Record
    | SelfPropsRow + ()
    )

type Props
  = As.Props PropsRow

type StateProps
  = Props -- because we don't have component references

type StatePropagatedProps
  = Record
      ( states :: StatesF PropagatedState Record
      | SelfPropsRow + ()
      )

type RequiredProps
  = As.RequiredProps RequiredPropsRow

type ResolvedComponentsProps
  = As.ResolvedComponentsProps ResolvedComponentsPropsRow

type PropagatedStatesProps
  = As.PropagatedStatesProps PropagatedStatesPropsRow

type ResolvedProps
  = As.ResolvedProps ResolvedPropsRow

type StateResolvedProps
  = ResolvedProps -- because we don't have component references

type ReferenceProps
  = As.ReferenceProps PropsRow

type ReferenceRequiredProps
  = As.ReferenceRequiredProps ReferenceRequiredPropsRow

type ReferenceResolvedComponentsProps
  = As.ReferenceResolvedComponentsProps ReferenceResolvedComponentsPropsRow

type ReferencePropagatedStatesProps
  = As.ReferencePropagatedStatesProps ReferencePropagatedStatesPropsRow

type ReferenceResolvedProps
  = As.ReferenceResolvedProps ResolvedPropsRow

-- | Compile-time check to ensure types are resolvable
correct :: CompileTimeCheck => Boolean
correct =
  isResolved' (any :: Props) (any :: ResolvedProps)
    && isResolved' (any :: ReferenceProps) (any :: ReferenceResolvedProps)
    && isResolved' (any :: StateProps) (any :: StateResolvedProps)
    && isRequiredProps' (any :: Props) (any :: RequiredProps)
    && isResolvedComponents' (any :: RequiredProps) (any :: ResolvedComponentsProps)
    && isResolvedStates' (any :: ResolvedComponentsProps) (any :: PropagatedStatesProps)
    && isResolvedValues' (any :: PropagatedStatesProps) (any :: ResolvedProps)
