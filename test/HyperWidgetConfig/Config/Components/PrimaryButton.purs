module Test.HyperWidgetConfig.Config.Components.PrimaryButton (spec) where

import Test.Prelude
import Data.SubRecord (mkSubRecord)
import HyperWidgetConfig.Config.Components.PrimaryButton as PrimaryButton
import HyperWidgetConfig.Reference (Reference(..))

spec :: Spec Unit
spec = do
  describe "Props" do
    pending' "should pass compile-time check" do
      let
        _ = mkSubRecord {} :: PrimaryButton.Props

        _ = mkSubRecord { margin: Literal 123 } :: PrimaryButton.Props

        _ =
          mkSubRecord
            { margin: Literal 123
            , states:
                mkSubRecord
                  { enabled:
                      mkSubRecord
                        { margin: Literal 123
                        }
                  }
            } ::
            PrimaryButton.Props
      pure unit
  describe "ResolvedProps" do
    pending' "should pass compile-time-check" do
      let
        _ =
          { margin: 123
          , states:
              { enabled:
                  { margin: 123
                  }
              }
          } ::
            PrimaryButton.ResolvedProps
      pure unit
  describe "ReferenceProps" do
    pending' "should pass compile-time check" do
      let
        _ = mkSubRecord {} :: PrimaryButton.ReferenceProps

        _ = mkSubRecord { margin: Literal 123 } :: PrimaryButton.ReferenceProps

        _ = mkSubRecord { component: "components.primaryButton", margin: Literal 123 } :: PrimaryButton.ReferenceProps
      pure unit
  describe "ReferenceResolvedProps" do
    pending' "should pass compile-time check" do
      let
        _ =
          { component: "components.primaryButton"
          , margin: 123
          , states:
              { enabled:
                  { margin: 123
                  }
              }
          } ::
            PrimaryButton.ReferenceResolvedProps
      pure unit
