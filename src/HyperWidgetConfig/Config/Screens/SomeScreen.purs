module HyperWidgetConfig.Config.Screens.SomeScreen where

import Prelude
import Data.Nullable (Nullable)
import Data.SubRecord (SubRecord)
import Extra.Debug (class CompileTimeCheck, any)
import HyperWidgetConfig.As as As
import HyperWidgetConfig.Config.Components.GridItem as GridItem
import HyperWidgetConfig.Config.Components.PrimaryButton as PrimaryButton
import HyperWidgetConfig.Config.RequireProps (isRequiredProps')
import HyperWidgetConfig.Reference (Reference)
import HyperWidgetConfig.Resolve.ResolveAll (isResolved')
import HyperWidgetConfig.Resolve.ResolveComponents (isResolvedComponents')
import HyperWidgetConfig.Resolve.ResolveStates (isResolvedStates')
import HyperWidgetConfig.Resolve.ResolveValues (isResolvedValues')
import Type.Row (type (+))

type SelfPropsRow rest
  = ( someScreenProp :: Reference Number | rest )

type ResolvedSelfPropsRow rest
  = ( someScreenProp :: Number | rest )

type ReferenceRequiredSelfPropsRow rest
  = ( someScreenProp :: Nullable (Reference Number) | rest )

type StatesF state f
  = f
      ( someScreenState :: state
      )

type State
  = SubRecord
      ( SelfPropsRow
          + ( gridItem :: GridItem.StateProps
          , primaryButton :: PrimaryButton.StateProps
          )
      )

type PropagatedState
  = Record
      ( SelfPropsRow
          + ( gridItem :: GridItem.StatePropagatedProps
          , primaryButton :: PrimaryButton.StatePropagatedProps
          )
      )

type ResolvedState
  = Record
      ( ResolvedSelfPropsRow
          + ( gridItem :: GridItem.StateResolvedProps
          , primaryButton :: PrimaryButton.StateResolvedProps
          )
      )

type PropsRow
  = ( states :: StatesF State SubRecord
    | SelfPropsRow
      + ( gridItem :: GridItem.ReferenceProps
      , primaryButton :: PrimaryButton.ReferenceProps
      )
    )

type RequiredPropsRow
  = ( states :: StatesF State SubRecord
    | SelfPropsRow
      + ( gridItem :: GridItem.ReferenceRequiredProps
      , primaryButton :: PrimaryButton.ReferenceRequiredProps
      )
    )

type ResolvedComponentsPropsRow
  = ( states :: StatesF State SubRecord
    | SelfPropsRow
      + ( gridItem :: GridItem.ReferenceResolvedComponentsProps
      , primaryButton :: PrimaryButton.ReferenceResolvedComponentsProps
      )
    )

type PropagatedStatesPropsRow
  = ( states :: StatesF PropagatedState Record
    | SelfPropsRow
      + ( gridItem :: GridItem.ReferencePropagatedStatesProps
      , primaryButton :: PrimaryButton.ReferencePropagatedStatesProps
      )
    )

type StatePropsRow
  = ( states :: StatesF State SubRecord
    | SelfPropsRow
      + ( gridItem :: GridItem.StateProps
      , primaryButton :: PrimaryButton.StateProps
      )
    )

type StatePropagatedPropsRow
  = ( states :: StatesF PropagatedState Record
    | SelfPropsRow
      + ( gridItem :: GridItem.StatePropagatedProps
      , primaryButton :: PrimaryButton.StatePropagatedProps
      )
    )

type ResolvedPropsRow
  = ( states :: StatesF ResolvedState Record
    | ResolvedSelfPropsRow
      + ( gridItem :: GridItem.ReferenceResolvedProps
      , primaryButton :: PrimaryButton.ReferenceResolvedProps
      )
    )

type ReferenceRequiredPropsRow
  = ( states :: Nullable (StatesF State SubRecord)
    | ReferenceRequiredSelfPropsRow
      + ( primaryButton :: Nullable (PrimaryButton.ReferenceProps)
      , gridItem :: Nullable (GridItem.ReferenceProps)
      )
    )

type ReferenceResolvedComponentsPropsRow
  = ( states :: StatesF ResolvedState Record
    | SelfPropsRow
      + ( gridItem :: GridItem.ReferenceResolvedComponentsProps
      , primaryButton :: PrimaryButton.ReferenceResolvedComponentsProps
      )
    )

type ReferencePropagatedStatesPropsRow
  = ( states :: StatesF PropagatedState Record
    | SelfPropsRow
      + ( gridItem :: GridItem.ReferencePropagatedStatesProps
      , primaryButton :: PrimaryButton.ReferencePropagatedStatesProps
      )
    )

type StateResolvedPropsRow
  = ( states :: StatesF ResolvedState Record
    | ResolvedSelfPropsRow
      + ( gridItem :: GridItem.StateResolvedProps
      , primaryButton :: PrimaryButton.StateResolvedProps
      )
    )

type Props
  = As.Props PropsRow

type StateProps
  = As.Props StatePropsRow

type StatePropagatedProps
  = As.StatePropagatedProps StatePropagatedPropsRow

type RequiredProps
  = As.RequiredProps RequiredPropsRow

type ResolvedComponentsProps
  = As.ResolvedComponentsProps ResolvedComponentsPropsRow

type PropagatedStatesProps
  = As.PropagatedStatesProps PropagatedStatesPropsRow

type ResolvedProps
  = As.ResolvedProps ResolvedPropsRow

type StateResolvedProps
  = As.ResolvedProps StateResolvedPropsRow

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
