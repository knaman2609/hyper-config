module Test.HyperWidgetConfig.Config.Containers.GridItem (spec) where

import Test.Prelude
import Data.SubRecord (mkSubRecord)
import HyperWidgetConfig.Config.Components.GridItem as GridItem
import HyperWidgetConfig.Reference (Reference(..))

spec :: Spec Unit
spec = do
  describe "Props" do
    it "should pass compile-time check" do
      let
        _ = mkSubRecord {} :: GridItem.Props

        _ = mkSubRecord { padding: Reference "themeConfig.styling.padding" } :: GridItem.Props

        _ =
          mkSubRecord
            { padding: Literal 123
            , primaryButton:
                mkSubRecord {}
            } ::
            GridItem.Props

        _ =
          mkSubRecord
            { padding: Literal 123
            , primaryButton:
                mkSubRecord
                  { margin: Literal 123
                  }
            } ::
            GridItem.Props
      pure unit
  describe "ResolvedProps" do
    it "should pass compile-time-check" do
      let
        _ =
          { padding: 123
          , primaryButton:
              { component: "components.primaryButton"
              , margin: 123
              , states:
                  { enabled:
                      { margin: 123
                      }
                  }
              }
          , states:
              { hidden:
                  { padding: 123
                  , primaryButton:
                      { margin: 123
                      , states:
                          { enabled:
                              { margin: 123
                              }
                          }
                      }
                  }
              }
          } ::
            GridItem.ResolvedProps
      pure unit
  describe "ReferenceProps" do
    it "should pass compile-time check" do
      let
        _ = mkSubRecord {} :: GridItem.ReferenceProps

        _ = mkSubRecord { padding: Literal 123 } :: GridItem.ReferenceProps

        _ = mkSubRecord { component: "containers.gridItem", padding: Literal 123 } :: GridItem.ReferenceProps
      pure unit
  describe "ReferenceResolvedProps" do
    it "should pass compile-time check" do
      let
        _ =
          { component: "containers.gridItem"
          , padding: 123
          , primaryButton:
              { component: "components.primaryButton"
              , margin: 123
              , states:
                  { enabled: { margin: 123 }
                  }
              }
          , states:
              { hidden:
                  { padding: 123
                  , primaryButton:
                      { margin: 123
                      , states:
                          { enabled:
                              { margin: 123
                              }
                          }
                      }
                  }
              }
          } ::
            GridItem.ReferenceResolvedProps
      pure unit
