module Test.HyperWidgetConfig.Resolve.ToConfigTree (spec) where

import Test.Prelude
import Data.Maybe (Maybe(..))
import Data.Nullable (null)
import Data.SubRecord (SubRecord, mkSubRecord)
import Data.Tuple (Tuple(..))
import Extra.Data.Lens.Indexed (fromPath)
import Foreign.Object (fromFoldable)
import Foreign.Object as Object
import HyperWidgetConfig.Config as Config
import HyperWidgetConfig.Config.Components.GridItem as GridItem
import HyperWidgetConfig.Config.Components.PrimaryButton as PrimaryButton
import HyperWidgetConfig.Config.Screens.SomeScreen as SomeScreen
import HyperWidgetConfig.Reference (Reference(..))
import HyperWidgetConfig.Resolve.ConfigTree.Types (ComponentDefinition(..), ComponentReference(..), ConfigTree(..), Props, PropsTree(..), StateProps(..), States)
import HyperWidgetConfig.Resolve.ConfigTree.Value (mkValue)
import HyperWidgetConfig.Resolve.ToConfigTree (toComponentDefinition, toComponentReference, toComponentReferences, toConfigTree, toProps, toStateProps, toStates)
import Test.Spec.Assertions (shouldEqual)

exampleRequiredConfig :: Config.RequiredProps
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
          { someScreenProp: Reference "qwer"
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

spec :: Spec Unit
spec = do
  describe "toProps" do
    it "should pass for component substates" do
      let
        input :: PrimaryButton.State
        input = mkSubRecord {}

        result = toProps input

        expected :: Props
        expected = Object.empty
      result `shouldEqual` expected
    it "should pass for component" do
      let
        input :: PrimaryButton.RequiredProps
        input =
          { margin: Literal 123
          , states: mkSubRecord {} :: PrimaryButton.StatesF PrimaryButton.State SubRecord
          }

        result = toProps input

        expected :: Props
        expected =
          Object.fromHomogeneous
            { margin: Prop (mkValue (Literal 123))
            }
      result `shouldEqual` expected
    it "should pass for container" do
      let
        input :: GridItem.RequiredProps
        input =
          { padding: Literal 123
          , primaryButton:
              { component: "components.primaryButton"
              , margin: null
              , states: null
              }
          , states: mkSubRecord {} :: GridItem.StatesF GridItem.State SubRecord
          }

        result = toProps input

        expected :: Props
        expected =
          Object.fromHomogeneous
            { padding: Prop (mkValue (Literal 123))
            }
      result `shouldEqual` expected
    it "should pass for screen" do
      let
        input :: SomeScreen.RequiredProps
        input =
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

        result = toProps input

        expected :: Props
        expected =
          Object.fromHomogeneous
            { someScreenProp: Prop (mkValue (Literal 123.0))
            }
      result `shouldEqual` expected
  describe "toStateProps" do
    it "should pass for component" do
      let
        input :: PrimaryButton.State
        input = mkSubRecord {}

        result = toStateProps input

        expected :: StateProps
        expected =
          StateProps
            { props: Object.empty
            , subComponents: Object.empty
            }
      result `shouldEqual` expected
  describe "toStates" do
    it "should pass for component" do
      let
        input :: PrimaryButton.StatesF PrimaryButton.State SubRecord
        input = mkSubRecord {}

        result = toStates input

        expected :: States
        expected = Object.fromHomogeneous {}
      result `shouldEqual` expected
  describe "toComponentReference" do
    it "should pass for component" do
      let
        input :: PrimaryButton.ReferenceRequiredProps
        input =
          { component: "components.primaryButton"
          , margin: null
          , states: null
          }

        result = toComponentReference input (fromPath "containers.gridItem.primaryButton")

        expected :: ComponentReference
        expected =
          ComponentReference
            { component:
                { identifier: "components.primaryButton"
                , reference: Just "components.primaryButton"
                }
            , props: Object.empty
            , states: Object.empty
            , subComponents: Object.empty
            }
      result `shouldEqual` expected
    it "should pass for container" do
      let
        input :: GridItem.ReferenceRequiredProps
        input =
          { component: "containers.gridItem"
          , padding: null
          , primaryButton: null
          , states: null
          }

        result = toComponentReference input (fromPath "screens.someScreen.gridItem")

        expected :: ComponentReference
        expected =
          ComponentReference
            { component:
                { identifier: "containers.gridItem"
                , reference: Just "containers.gridItem"
                }
            , props: Object.empty
            , states: Object.empty
            , subComponents: Object.empty
            }
      result `shouldEqual` expected
  describe "toComponentReferences" do
    it "should pass for component" do
      let
        input :: PrimaryButton.RequiredProps
        input =
          { margin: Literal 123
          , states: mkSubRecord {} :: PrimaryButton.StatesF PrimaryButton.State SubRecord
          }

        result = toComponentReferences input (fromPath "components.primaryButton")

        expected = Object.empty
      result `shouldEqual` expected
    it "should pass for container" do
      let
        input :: GridItem.RequiredProps
        input =
          { padding: Literal 123
          , primaryButton:
              { component: "components.primaryButton"
              , margin: null
              , states: null
              }
          , states: mkSubRecord {} :: GridItem.StatesF GridItem.State SubRecord
          }

        result = toComponentReferences input (fromPath "containers.gridItem")

        expected =
          Object.fromHomogeneous
            { primaryButton:
                ComponentReference
                  { component:
                      { identifier: "components.primaryButton"
                      , reference: Just "components.primaryButton"
                      }
                  , props: Object.empty
                  , states: Object.empty
                  , subComponents: Object.empty
                  }
            }
      result `shouldEqual` expected
  describe "toComponentDefinition" do
    it "should pass for component" do
      let
        input :: PrimaryButton.RequiredProps
        input =
          { margin: Literal 123
          , states: mkSubRecord {} :: PrimaryButton.StatesF PrimaryButton.State SubRecord
          }

        result = toComponentDefinition input (fromPath "components.primaryButton")

        expected =
          ComponentDefinition
            { props:
                Object.fromHomogeneous
                  { margin: Prop $ mkValue (Literal 123)
                  }
            , states: Object.fromHomogeneous {}
            , subComponents: Object.fromHomogeneous {}
            , identifier: "components.primaryButton"
            }
      result `shouldEqual` expected
    it "should pass for container" do
      let
        input :: GridItem.RequiredProps
        input =
          { padding: Literal 123
          , primaryButton:
              { component: "components.primaryButton"
              , margin: null
              , states: null
              }
          , states: mkSubRecord {} :: GridItem.StatesF GridItem.State SubRecord
          }

        result = toComponentDefinition input (fromPath "containers.gridItem")

        expected =
          ComponentDefinition
            { props:
                Object.fromHomogeneous
                  { padding: Prop $ mkValue (Literal 123)
                  }
            , states: Object.fromHomogeneous {}
            , subComponents:
                Object.fromHomogeneous
                  { primaryButton:
                      ComponentReference
                        { component:
                            { identifier: "components.primaryButton"
                            , reference: Just "components.primaryButton"
                            }
                        , props: Object.empty
                        , states: Object.empty
                        , subComponents: Object.empty
                        }
                  }
            , identifier: "containers.gridItem"
            }
      result `shouldEqual` expected
    it "should pass for screen" do
      let
        input :: SomeScreen.RequiredProps
        input =
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

        result = toComponentDefinition input (fromPath "screens.someScreen")

        expected =
          ComponentDefinition
            { props:
                Object.fromHomogeneous
                  { someScreenProp: Prop $ mkValue (Literal 123.0)
                  }
            , states: Object.empty
            , identifier: "screens.someScreen"
            , subComponents:
                Object.fromHomogeneous
                  { primaryButton:
                      ComponentReference
                        { component:
                            { identifier: "components.primaryButton"
                            , reference: Just "components.primaryButton"
                            }
                        , props: Object.empty
                        , states: Object.empty
                        , subComponents: Object.empty
                        }
                  , gridItem:
                      ComponentReference
                        { component:
                            { identifier: "containers.gridItem"
                            , reference: Just "containers.gridItem"
                            }
                        , props: Object.empty
                        , states: Object.empty
                        , subComponents: Object.empty
                        }
                  }
            }
      result `shouldEqual` expected
  describe "toConfigTree" do
    it "should pass for section" do
      let
        input =
          { padding: 1
          , margin: 12
          }

        result = toConfigTree input (fromPath "themeConfig")

        expected =
          ( Section
              ( fromFoldable
                  [ (Tuple "margin" (Value (mkValue 12)))
                  , (Tuple "padding" (Value (mkValue 1)))
                  ]
              )
          )
      result `shouldEqual` expected
    it "should pass for component" do
      let
        input :: PrimaryButton.RequiredProps
        input =
          { margin: Literal 123
          , states: mkSubRecord {} :: PrimaryButton.StatesF PrimaryButton.State SubRecord
          }

        result = toConfigTree input (fromPath "components.primaryButton")

        expected =
          Component
            ( ComponentDefinition
                { identifier: "components.primaryButton"
                , props: (fromFoldable [ (Tuple "margin" (Prop $ mkValue (Literal 123))) ])
                , states: (fromFoldable [])
                , subComponents: (fromFoldable [])
                }
            )
      result `shouldEqual` expected
    it "should pass for container" do
      let
        input :: GridItem.RequiredProps
        input =
          { padding: Literal 123
          , primaryButton:
              { component: "components.primaryButton"
              , margin: null
              , states: null
              }
          , states: mkSubRecord {} :: GridItem.StatesF GridItem.State SubRecord
          }

        result = toConfigTree input (fromPath "containers.gridItem")

        expected =
          Component
            ( ComponentDefinition
                { identifier: "containers.gridItem"
                , props: (fromFoldable [ (Tuple "padding" (Prop $ mkValue (Literal 123))) ])
                , states: (fromFoldable [])
                , subComponents:
                    ( fromFoldable
                        [ ( Tuple "primaryButton"
                              ( ComponentReference
                                  { component:
                                      { identifier: "components.primaryButton"
                                      , reference: (Just "components.primaryButton")
                                      }
                                  , props: (fromFoldable [])
                                  , states: (fromFoldable [])
                                  , subComponents: (fromFoldable [])
                                  }
                              )
                          )
                        ]
                    )
                }
            )
      result `shouldEqual` expected
    it "should pass for config" do
      let
        input :: Config.RequiredProps
        input = exampleRequiredConfig

        result = toConfigTree input mempty

        expected =
          Section
            ( fromFoldable
                [ ( Tuple "themeConfig"
                      ( Section
                          ( fromFoldable
                              [ (Tuple "padding" (Value $ mkValue 1)), (Tuple "margin" (Value $ mkValue 12))
                              ]
                          )
                      )
                  )
                , ( Tuple "screens"
                      ( Section
                          ( fromFoldable
                              [ ( Tuple "someScreen"
                                    ( Component
                                        ( ComponentDefinition
                                            { identifier: "screens.someScreen"
                                            , props: (fromFoldable [ (Tuple "someScreenProp" (Prop $ mkValue (Reference "qwer" :: Reference Number))) ])
                                            , states: (fromFoldable [])
                                            , subComponents:
                                                ( fromFoldable
                                                    [ ( Tuple "primaryButton"
                                                          ( ComponentReference
                                                              { component:
                                                                  { identifier: "components.primaryButton"
                                                                  , reference: (Just "components.primaryButton")
                                                                  }
                                                              , props: (fromFoldable [])
                                                              , states: (fromFoldable [])
                                                              , subComponents: (fromFoldable [])
                                                              }
                                                          )
                                                      )
                                                    , ( Tuple "gridItem"
                                                          ( ComponentReference
                                                              { component:
                                                                  { identifier: "containers.gridItem"
                                                                  , reference: (Just "containers.gridItem")
                                                                  }
                                                              , props: (fromFoldable [])
                                                              , states: (fromFoldable [])
                                                              , subComponents: (fromFoldable [])
                                                              }
                                                          )
                                                      )
                                                    ]
                                                )
                                            }
                                        )
                                    )
                                )
                              ]
                          )
                      )
                  )
                , ( Tuple "containers"
                      ( Section
                          ( fromFoldable
                              [ ( Tuple "gridItem"
                                    ( Component
                                        ( ComponentDefinition
                                            { identifier: "containers.gridItem"
                                            , props: (fromFoldable [ (Tuple "padding" (Prop $ mkValue (Literal 123))) ])
                                            , states: (fromFoldable [])
                                            , subComponents:
                                                ( fromFoldable
                                                    [ ( Tuple "primaryButton"
                                                          ( ComponentReference
                                                              { component:
                                                                  { identifier: "components.primaryButton"
                                                                  , reference: (Just "components.primaryButton")
                                                                  }
                                                              , props: (fromFoldable [])
                                                              , states: (fromFoldable [])
                                                              , subComponents: (fromFoldable [])
                                                              }
                                                          )
                                                      )
                                                    ]
                                                )
                                            }
                                        )
                                    )
                                )
                              ]
                          )
                      )
                  )
                , ( Tuple "components"
                      ( Section
                          ( fromFoldable
                              [ ( Tuple "primaryButton"
                                    ( Component
                                        ( ComponentDefinition
                                            { identifier: "components.primaryButton"
                                            , props: (fromFoldable [ (Tuple "margin" (Prop $ mkValue (Literal 123))) ])
                                            , states: (fromFoldable [])
                                            , subComponents: (fromFoldable [])
                                            }
                                        )
                                    )
                                )
                              ]
                          )
                      )
                  )
                ]
            )
      result `shouldEqual` expected
