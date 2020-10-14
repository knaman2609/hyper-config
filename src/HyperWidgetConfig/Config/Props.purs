module HyperWidgetConfig.Config.Props where

import Prelude
import Data.SubRecord (SubRecord)
import Extra.Debug (class CompileTimeCheck, any)
import HyperWidgetConfig.As as As
import HyperWidgetConfig.Config.Components.GridItem as GridItem
import HyperWidgetConfig.Config.Components.PrimaryButton as PrimaryButton
import HyperWidgetConfig.Config.RequireProps (class RequireProps_)
import HyperWidgetConfig.Config.Screens.SomeScreen as SomeScreen
import HyperWidgetConfig.Resolve.ResolveAll (isResolved')
import HyperWidgetConfig.Resolve.ResolveComponents (isResolvedComponents')
import HyperWidgetConfig.Resolve.ResolveStates (isResolvedStates')
import HyperWidgetConfig.Resolve.ResolveValues (isResolvedValues')

type PropsRow
  = ( themeConfig ::
        SubRecord
          ( padding :: Int
          , margin :: Int
          )
    , components ::
        SubRecord
          ( primaryButton :: PrimaryButton.Props
          )
    , containers ::
        SubRecord
          ( gridItem :: GridItem.Props
          )
    , screens ::
        SubRecord
          ( someScreen :: SomeScreen.Props
          )
    )

type RequiredPropsRow
  = ( themeConfig ::
        Record
          ( padding :: Int
          , margin :: Int
          )
    , components ::
        Record
          ( primaryButton :: PrimaryButton.RequiredProps
          )
    , containers ::
        Record
          ( gridItem :: GridItem.RequiredProps
          )
    , screens ::
        Record
          ( someScreen :: SomeScreen.RequiredProps
          )
    )

type ResolvedComponentsPropsRow
  = ( themeConfig ::
        Record
          ( padding :: Int
          , margin :: Int
          )
    , components ::
        Record
          ( primaryButton :: PrimaryButton.ResolvedComponentsProps
          )
    , containers ::
        Record
          ( gridItem :: GridItem.ResolvedComponentsProps
          )
    , screens ::
        Record
          ( someScreen :: SomeScreen.ResolvedComponentsProps
          )
    )

type PropagatedStatesPropsRow
  = ( themeConfig ::
        Record
          ( padding :: Int
          , margin :: Int
          )
    , components ::
        Record
          ( primaryButton :: PrimaryButton.PropagatedStatesProps
          )
    , containers ::
        Record
          ( gridItem :: GridItem.PropagatedStatesProps
          )
    , screens ::
        Record
          ( someScreen :: SomeScreen.PropagatedStatesProps
          )
    )

type ResolvedPropsRow
  = ( themeConfig ::
        Record
          ( padding :: Int
          , margin :: Int
          )
    , components ::
        Record
          ( primaryButton :: PrimaryButton.ResolvedProps
          )
    , containers ::
        Record
          ( gridItem :: GridItem.ResolvedProps
          )
    , screens ::
        Record
          ( someScreen :: SomeScreen.ResolvedProps
          )
    )

type Props
  = As.Props PropsRow

type ResolvedProps
  = As.ResolvedProps ResolvedPropsRow

type ResolvedComponentsProps
  = As.ResolvedComponentsProps ResolvedComponentsPropsRow

type PropagatedStatesProps
  = As.PropagatedStatesProps PropagatedStatesPropsRow

type RequiredProps
  = As.RequiredProps RequiredPropsRow

isRequiredProps :: forall required. CompileTimeCheck => RequireProps required => required -> Boolean
isRequiredProps _ = true

class RequireProps required

instance requiredProps :: RequireProps_ Props required => RequireProps required

correct :: CompileTimeCheck => Boolean
correct =
  isResolved' (any :: Props) (any :: ResolvedProps)
    && isRequiredProps (any :: RequiredProps)
    && isResolvedComponents' (any :: RequiredProps) (any :: ResolvedComponentsProps)
    && isResolvedStates' (any :: ResolvedComponentsProps) (any :: PropagatedStatesProps)
    && isResolvedValues' (any :: PropagatedStatesProps) (any :: ResolvedProps)
