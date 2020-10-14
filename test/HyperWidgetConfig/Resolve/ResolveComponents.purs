module Test.HyperWidgetConfig.Resolve.ResolveComponents (spec) where

import Test.Prelude
import Data.Nullable (Nullable, null)
import Data.SubRecord (SubRecord, mkSubRecord)
import Data.Validation.Semigroup (V)
import HyperWidgetConfig.Config as Config
import HyperWidgetConfig.Config.Components.GridItem as GridItem
import HyperWidgetConfig.Config.Components.PrimaryButton as PrimaryButton
import HyperWidgetConfig.Config.Screens.SomeScreen as SomeScreen
import HyperWidgetConfig.Error (ConfigError)
import HyperWidgetConfig.Reference (Reference(..))
import HyperWidgetConfig.Resolve (applyDefaultConfig, resolveComponents)
import HyperWidgetConfig.Resolve.ResolveComponents (class ResolveComponentsRowList, class ResolveComponents_)
import Prim.Boolean (False, True)
import Prim.RowList as RL
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  let
    exampleRequiredConfig =
      { themeConfig:
          { padding: 1
          , margin: 12
          }
      , components:
          { primaryButton:
              { margin: Literal 123
              , states: mkSubRecord {} :: PrimaryButton.StatesF PrimaryButton.State SubRecord
              }
          }
      , containers:
          { gridItem:
              { padding: Literal 123
              , primaryButton:
                  { component: "components.primaryButton"
                  , margin: null
                  , states: null
                  }
              , states: mkSubRecord {} :: GridItem.StatesF GridItem.State SubRecord
              }
          }
      , screens:
          { someScreen:
              { someScreenProp: Literal 123.0
              , primaryButton:
                  { component: "components.primaryButton"
                  , margin: null
                  , states: null
                  }
              , gridItem:
                  { component: "containers.gridItem"
                  , padding: null
                  , primaryButton: null
                  , states: null
                  }
              , states: mkSubRecord {} :: SomeScreen.StatesF SomeScreen.State SubRecord
              }
          }
      }
  describe "ResolveComponents_" do
    it "should pass" do
      let
        _ = runCompileTimeCheck (any :: ResolveComponents_ { margin :: Reference Int } { margin :: Reference Int } => Unit)

        _ =
          runCompileTimeCheck
            ( any ::
                ResolveComponents_
                  { containers ::
                      { gridItem ::
                          { padding :: Reference Int
                          , primaryButton ::
                              { component :: String
                              , margin :: Nullable Int
                              }
                          }
                      }
                  }
                  { containers ::
                      { gridItem ::
                          { padding :: Reference Int
                          , primaryButton ::
                              { component :: String
                              , margin :: Int
                              }
                          }
                      }
                  } =>
                Unit
            )

        result = resolveComponents (applyDefaultConfig exampleRequiredConfig (mkSubRecord {}))

        expected :: V (Array ConfigError) (Config.ResolvedComponentsProps)
        expected =
          pure
            { themeConfig:
                { padding: 1
                , margin: 12
                }
            , components:
                { primaryButton:
                    { margin: Literal 123
                    , states: mkSubRecord {} :: PrimaryButton.StatesF PrimaryButton.State SubRecord
                    }
                }
            , containers:
                { gridItem:
                    { padding: Literal 123
                    , primaryButton:
                        { component: "components.primaryButton"
                        , margin: Literal 123
                        , states: mkSubRecord {} :: PrimaryButton.StatesF PrimaryButton.State SubRecord
                        }
                    , states: mkSubRecord {} :: GridItem.StatesF GridItem.State SubRecord
                    }
                }
            , screens:
                { someScreen:
                    { someScreenProp: Literal 123.0
                    , primaryButton:
                        { component: "components.primaryButton"
                        , margin: Literal 123
                        , states: mkSubRecord {} :: PrimaryButton.StatesF PrimaryButton.State SubRecord
                        }
                    , gridItem:
                        { component: "containers.gridItem"
                        , padding: Literal 123
                        , primaryButton:
                            { component: "components.primaryButton"
                            , margin: Literal 123
                            , states: mkSubRecord {} :: PrimaryButton.StatesF PrimaryButton.State SubRecord
                            }
                        , states: mkSubRecord {} :: GridItem.StatesF GridItem.State SubRecord
                        }
                    , states: mkSubRecord {} :: SomeScreen.StatesF SomeScreen.State SubRecord
                    }
                }
            }
      result `shouldEqual` expected
  describe "ResolveComponentsRowList" do
    itCompileTimeCheck "should pass" do
      let
        _ = runCompileTimeCheck (any :: ResolveComponentsRowList False (RL.Cons "test" String RL.Nil) ( test :: String ) => Unit)

        _ = runCompileTimeCheck (any :: ResolveComponentsRowList True (RL.Cons "states" (SubRecord ()) RL.Nil) ( states :: SubRecord () ) => Unit)

        _ = runCompileTimeCheck (any :: ResolveComponentsRowList True (RL.Cons "margin" (Nullable (Reference Int)) RL.Nil) ( margin :: Reference Int ) => Unit)
      pure unit
