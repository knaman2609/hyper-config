module Test.HyperWidgetConfig.Resolve (spec) where

import Test.Prelude
import Control.Monad.Except (Except, runExcept, throwError)
import Data.Either (fromLeft, fromRight)
import Data.Nullable (notNull, null)
import Data.SubRecord (SubRecord, mkSubRecord)
import Data.SubRecord.Heterogeneous (mkSubRecordRecursive)
import Data.Tuple (Tuple(..))
import Data.Validation.Semigroup (V, invalid)
import Foreign.Object (fromFoldable)
import HyperWidgetConfig.Config (isRequiredProps)
import HyperWidgetConfig.Config as Config
import HyperWidgetConfig.Config.Components.GridItem as GridItem
import HyperWidgetConfig.Config.Components.PrimaryButton as PrimaryButton
import HyperWidgetConfig.Config.Screens.SomeScreen as SomeScreen
import HyperWidgetConfig.Error (ReferenceErrorEnvelope, ResolveError(..), ConfigError, packReferenceError)
import HyperWidgetConfig.Reference (Reference(..))
import HyperWidgetConfig.Reify (RuntimeType(..))
import HyperWidgetConfig.Resolve (applyDefaultConfig, forAllAttachPropertyInfo, resolveComponents, resolveConfig, resolveValue)
import HyperWidgetConfig.Resolve.ResolveStates (resolveStates')
import Partial.Unsafe (unsafePartial)
import Test.Spec.Assertions (shouldEqual)

exampleParent :: Config.ResolvedProps
exampleParent =
  { themeConfig:
      { padding: 1
      , margin: 12
      }
  , components:
      { primaryButton:
          { margin: 123
          , states:
              { enabled:
                  { margin: 222
                  }
              }
          }
      }
  , containers:
      { gridItem:
          { padding: 3210
          , primaryButton:
              { component: "components.primaryButton"
              , margin: 1234
              , states:
                  { enabled:
                      { margin: 222
                      }
                  }
              }
          , states:
              { hidden:
                  { padding: 3210
                  , primaryButton:
                      { margin: 1234
                      , states:
                          { enabled:
                              { margin: 222
                              }
                          }
                      }
                  }
              }
          }
      }
  , screens:
      { someScreen:
          { someScreenProp: 123.0
          , primaryButton:
              { component: "components.primaryButton"
              , margin: 123
              , states:
                  { enabled:
                      { margin: 222
                      }
                  }
              }
          , gridItem:
              { component: "containers.gridItem"
              , padding: 3210
              , primaryButton:
                  { component: "components.primaryButton"
                  , margin: 1234
                  , states:
                      { enabled:
                          { margin: 222
                          }
                      }
                  }
              , states:
                  { hidden:
                      { padding: 3210
                      , primaryButton:
                          { margin: 1234
                          , states:
                              { enabled:
                                  { margin: 222
                                  }
                              }
                          }
                      }
                  }
              }
          , states:
              { someScreenState:
                  { someScreenProp: 123.0
                  , primaryButton:
                      { margin: 123
                      , states:
                          { enabled:
                              { margin: 222
                              }
                          }
                      }
                  , gridItem:
                      { padding: 3210
                      , primaryButton:
                          { margin: 1234
                          , states:
                              { enabled:
                                  { margin: 222
                                  }
                              }
                          }
                      , states:
                          { hidden:
                              { padding: 3210
                              , primaryButton:
                                  { margin: 1234
                                  , states:
                                      { enabled:
                                          { margin: 222
                                          }
                                      }
                                  }
                              }
                          }
                      }
                  }
              }
          }
      }
  }

examplePropagatedStates :: Config.PropagatedStatesProps
examplePropagatedStates =
  { themeConfig:
      { padding: 1
      , margin: 12
      }
  , components:
      { primaryButton:
          { margin: Literal 123
          , states:
              { enabled:
                  { margin: Literal 222
                  }
              }
          }
      }
  , containers:
      { gridItem:
          { padding: Literal 3210
          , primaryButton:
              { component: "components.primaryButton"
              , margin: Literal 1234
              , states:
                  { enabled:
                      { margin: Literal 222
                      }
                  }
              }
          , states:
              { hidden:
                  { padding: Literal 3210
                  , primaryButton:
                      { margin: Literal 1234
                      , states:
                          { enabled:
                              { margin: Literal 222
                              }
                          }
                      }
                  }
              }
          }
      }
  , screens:
      { someScreen:
          { someScreenProp: Literal 123.0
          , primaryButton:
              { component: "components.primaryButton"
              , margin: Literal 123
              , states:
                  { enabled:
                      { margin: Literal 222
                      }
                  }
              }
          , gridItem:
              { component: "containers.gridItem"
              , padding: Literal 3210
              , primaryButton:
                  { component: "components.primaryButton"
                  , margin: Literal 1234
                  , states:
                      { enabled:
                          { margin: Literal 222
                          }
                      }
                  }
              , states:
                  { hidden:
                      { padding: Literal 3210
                      , primaryButton:
                          { margin: Literal 1234
                          , states:
                              { enabled:
                                  { margin: Literal 222
                                  }
                              }
                          }
                      }
                  }
              }
          , states:
              { someScreenState:
                  { someScreenProp: Literal 123.0
                  , primaryButton:
                      { margin: Literal 123
                      , states:
                          { enabled:
                              { margin: Literal 222
                              }
                          }
                      }
                  , gridItem:
                      { padding: Literal 3210
                      , primaryButton:
                          { margin: Literal 1234
                          , states:
                              { enabled:
                                  { margin: Literal 222
                                  }
                              }
                          }
                      , states:
                          { hidden:
                              { padding: Literal 3210
                              , primaryButton:
                                  { margin: Literal 1234
                                  , states:
                                      { enabled:
                                          { margin: Literal 222
                                          }
                                      }
                                  }
                              }
                          }
                      }
                  }
              }
          }
      }
  }

exampleConfig :: Config.Props
exampleConfig =
  mkSubRecordRecursive
    { themeConfig:
        { padding: 123
        }
    , components:
        { primaryButton:
            { margin: Literal 444
            }
        }
    , containers:
        { gridItem:
            { padding: Literal 321
            , primaryButton:
                { component: "components.primaryButton"
                , margin: Reference "themeConfig.margin"
                }
            }
        }
    }

spec :: Spec Unit
spec = do
  let
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
  itCompileTimeCheck "exampleRequiredConfig should pass compile-time check" do
    let
      _ = isRequiredProps exampleRequiredConfig
    pure unit
  describe "applyDefaultConfig" do
    it "should pass example" do
      let
        env = exampleRequiredConfig

        input :: Config.Props
        input = exampleConfig

        result = applyDefaultConfig env input

        expected =
          { themeConfig:
              { padding: 123
              , margin: 12
              }
          , components:
              { primaryButton:
                  { margin: Literal 444
                  , states: mkSubRecord {}
                  }
              }
          , containers:
              { gridItem:
                  { padding: Literal 321
                  , primaryButton:
                      { component: "components.primaryButton"
                      -- TODO: get rid of this `notNull`
                      , margin: notNull $ Reference "themeConfig.margin"
                      , states: null
                      }
                  , states: mkSubRecord {}
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
                  , states: mkSubRecord {}
                  }
              }
          }
      result `shouldEqual` expected
  describe "resolveValue" do
    let
      success :: forall e a. Except e a -> a
      success = unsafePartial fromRight <<< runExcept

      failure :: forall e a. Except e a -> e
      failure = unsafePartial fromLeft <<< runExcept
    it "should resolve literal value" do
      let
        result = resolveValue examplePropagatedStates (Literal 123)

        expected = 123
      success result `shouldEqual` expected
    it "should not resolve reference value with non-existent reference path" do
      let
        result = resolveValue examplePropagatedStates (Reference "qwer" :: Reference String)

        expected = { reference: "qwer", error: NotFound }
      failure result `shouldEqual` expected
    it "should not resolve reference value if intermediately point to value" do
      let
        result = resolveValue examplePropagatedStates (Reference "themeConfig.padding.someInner" :: Reference Int)

        expected = { reference: "themeConfig.padding.someInner", error: Conflict { conflictAt: "themeConfig.padding" } }
      failure result `shouldEqual` expected
    it "should not resolve reference value if point to wrong type" do
      let
        result = resolveValue examplePropagatedStates (Reference "themeConfig.margin" :: Reference String)

        expected =
          { reference: "themeConfig.margin"
          , error:
              WrongType
                { actual: RuntimeInt
                , expected: RuntimeString
                }
          }
      failure result `shouldEqual` expected
    it "should resolve reference value" do
      let
        result = resolveValue examplePropagatedStates (Reference "themeConfig.padding" :: Reference Int)

        expected = 1
      success result `shouldEqual` expected
    it "should resolve reference value to parent if missing in current" do
      let
        result = resolveValue examplePropagatedStates (Reference "themeConfig.margin" :: Reference Int)

        expected = 12
      success result `shouldEqual` expected
  describe "forAllAttachPropertyInfo" do
    it "should pass for empty subrecord" do
      let
        input = {}

        result = forAllAttachPropertyInfo input

        expected = {}
      result `shouldEqual` expected
    it "should skip non-error values" do
      let
        input = { int: 123 }

        result = forAllAttachPropertyInfo input

        expected = { int: 123 }
      result `shouldEqual` expected
    it "should skip non-error values recursively" do
      let
        input = { int: 123, inner: { innerInt: 321 } }

        result = forAllAttachPropertyInfo input

        expected = { int: 123, inner: { innerInt: 321 } }
      result `shouldEqual` expected
    it "should transform success values" do
      let
        input :: { notErr :: Except ReferenceErrorEnvelope Int }
        input = { notErr: pure 123 }

        result = forAllAttachPropertyInfo input

        expected = { notErr: pure 123 }
      result `shouldEqual` expected
    it "should transform error values" do
      let
        input :: { err :: Except ReferenceErrorEnvelope Int }
        input =
          { err:
              throwError $ packReferenceError NotFound "someReference"
          }

        result = forAllAttachPropertyInfo input

        expected =
          { err:
              invalid
                [ { reference: "someReference"
                  , path: "err"
                  , error: NotFound
                  }
                ]
          }
      result `shouldEqual` expected
    it "should transform success values recursively" do
      let
        input :: { inner :: { notErr :: Except ReferenceErrorEnvelope Int } }
        input = { inner: { notErr: pure 123 } }

        result = forAllAttachPropertyInfo input

        expected = { inner: { notErr: pure 123 } }
      result `shouldEqual` expected
    it "should transform error values recursively" do
      let
        input :: { inner :: { err :: Except ReferenceErrorEnvelope Int } }
        input =
          { inner:
              { err:
                  throwError $ packReferenceError NotFound "someReference"
              }
          }

        result = forAllAttachPropertyInfo input

        expected =
          { inner:
              { err:
                  invalid
                    [ { reference: "someReference"
                      , path: "inner.err" -- the path where error occured is correctly determined
                      , error: NotFound
                      }
                    ]
              }
          }
      result `shouldEqual` expected
    describe "resolveStates'" do
      it "should resolve single component" do
        let
          props ::
            Record
              ( foo :: Int
              , states ::
                  SubRecord
                    ( enabled :: SubRecord ( foo :: Int )
                    )
              )
          props =
            { foo: 123
            , states: mkSubRecord {}
            }

          result = resolveStates' props

          expected =
            { foo: 123
            , states:
                { enabled: { foo: 123 }
                }
            }
        result `shouldEqual` expected
      it "should resolve container component" do
        let
          props ::
            Record
              ( foo :: Int
              , subComponent ::
                  Record
                    ( bar :: String
                    , states ::
                        SubRecord
                          ( someState :: SubRecord ( bar :: String )
                          )
                    )
              , states ::
                  SubRecord
                    ( enabled ::
                        SubRecord
                          ( foo :: Int
                          , subComponent ::
                              SubRecord
                                ( bar :: String
                                , states ::
                                    SubRecord
                                      ( someState :: SubRecord ( bar :: String )
                                      )
                                )
                          )
                    )
              )
          props =
            { foo: 123
            , subComponent:
                { bar: "in the main props"
                , states: mkSubRecord {}
                }
            , states:
                mkSubRecord
                  { enabled:
                      mkSubRecord
                        { subComponent:
                            mkSubRecord
                              { bar: "in the main states"
                              }
                        }
                  }
            }

          result = resolveStates' props

          expected =
            { foo: 123
            , states:
                { enabled:
                    { foo: 123
                    , subComponent:
                        { bar: "in the main states"
                        , states:
                            { someState:
                                { bar: "in the main states"
                                }
                            }
                        }
                    }
                }
            , subComponent:
                { bar: "in the main props"
                , states:
                    { someState:
                        { bar: "in the main props"
                        }
                    }
                }
            }
        result `shouldEqual` expected
      it "should work with ApplyDefaultConfig + ResolveComponents" do
        let
          input :: Config.Props
          input = mkSubRecord {}

          resolvedComponents :: V (Array ConfigError) Config.ResolvedComponentsProps
          resolvedComponents = resolveComponents (applyDefaultConfig exampleRequiredConfig input)

          result = resolveStates' <$> resolvedComponents

          expected =
            pure
              { themeConfig:
                  { padding: 1
                  , margin: 12
                  }
              , components:
                  { primaryButton:
                      { margin: Literal 123
                      , states:
                          { enabled:
                              { margin: Literal 123
                              }
                          }
                      }
                  }
              , containers:
                  { gridItem:
                      { padding: Literal 123
                      , primaryButton:
                          { component: "components.primaryButton"
                          , margin: Literal 123
                          , states:
                              { enabled:
                                  { margin: Literal 123
                                  }
                              }
                          }
                      , states:
                          { hidden:
                              { padding: Literal 123
                              , primaryButton:
                                  { margin: Literal 123
                                  , states:
                                      { enabled:
                                          { margin: Literal 123
                                          }
                                      }
                                  }
                              }
                          }
                      }
                  }
              , screens:
                  { someScreen:
                      { someScreenProp: Literal 123.0
                      , primaryButton:
                          { component: "components.primaryButton"
                          , margin: Literal 123
                          , states:
                              { enabled:
                                  { margin: Literal 123
                                  }
                              }
                          }
                      , gridItem:
                          { component: "containers.gridItem"
                          , padding: Literal 123
                          , primaryButton:
                              { component: "components.primaryButton"
                              , margin: Literal 123
                              , states:
                                  { enabled:
                                      { margin: Literal 123
                                      }
                                  }
                              }
                          , states:
                              { hidden:
                                  { padding: Literal 123
                                  , primaryButton:
                                      { margin: Literal 123
                                      , states:
                                          { enabled:
                                              { margin: Literal 123
                                              }
                                          }
                                      }
                                  }
                              }
                          }
                      , states:
                          { someScreenState:
                              { someScreenProp: Literal 123.0
                              , primaryButton:
                                  { margin: Literal 123
                                  , states:
                                      { enabled:
                                          { margin: Literal 123
                                          }
                                      }
                                  }
                              , gridItem:
                                  { padding: Literal 123
                                  , primaryButton:
                                      { margin: Literal 123
                                      , states:
                                          { enabled:
                                              { margin: Literal 123
                                              }
                                          }
                                      }
                                  , states:
                                      { hidden:
                                          { padding: Literal 123
                                          , primaryButton:
                                              { margin: Literal 123
                                              , states:
                                                  { enabled:
                                                      { margin: Literal 123
                                                      }
                                                  }
                                              }
                                          }
                                      }
                                  }
                              }
                          }
                      }
                  }
              }
        result `shouldEqual` expected
    describe "resolveConfig" do
      it "should resolve example config" do
        let
          result :: V (Array ConfigError) Config.ResolvedProps
          result = resolveConfig exampleRequiredConfig exampleConfig

          expected =
            pure
              ( { components:
                    { primaryButton:
                        { margin: 444, states: { enabled: { margin: 444 } }
                        }
                    }
                , containers:
                    { gridItem:
                        { padding: 321
                        , primaryButton:
                            { component: "components.primaryButton"
                            , margin: 12
                            , states: { enabled: { margin: 12 } }
                            }
                        , states:
                            { hidden:
                                { padding: 321
                                , primaryButton:
                                    { margin: 12, states: { enabled: { margin: 12 } } }
                                }
                            }
                        }
                    }
                , screens:
                    { someScreen:
                        { gridItem:
                            { component: "containers.gridItem"
                            , padding: 321
                            , primaryButton:
                                { component: "components.primaryButton"
                                , margin: 12
                                , states: { enabled: { margin: 12 } }
                                }
                            , states:
                                { hidden:
                                    { padding: 321
                                    , primaryButton:
                                        { margin: 12, states: { enabled: { margin: 12 } }
                                        }
                                    }
                                }
                            }
                        , primaryButton:
                            { component: "components.primaryButton"
                            , margin: 444
                            , states: { enabled: { margin: 444 } }
                            }
                        , someScreenProp: 123.0
                        , states:
                            { someScreenState:
                                { gridItem:
                                    { padding: 321
                                    , primaryButton: { margin: 12, states: { enabled: { margin: 12 } } }
                                    , states:
                                        { hidden:
                                            { padding: 321
                                            , primaryButton:
                                                { margin: 12
                                                , states: { enabled: { margin: 12 } }
                                                }
                                            }
                                        }
                                    }
                                , primaryButton: { margin: 444, states: { enabled: { margin: 444 } } }
                                , someScreenProp: 123.0
                                }
                            }
                        }
                    }
                , themeConfig: { margin: 12, padding: 123 }
                }
              )
        result `shouldEqual` expected
      it "should aggregate errors" do
        let
          input :: Config.Props
          input =
            mkSubRecordRecursive
              { components:
                  { primaryButton:
                      { margin: Reference "some.weird.non.existing.path"
                      }
                  }
              , containers:
                  { gridItem:
                      { padding: Reference "themeConfig"
                      }
                  }
              }

          result :: V (Array ConfigError) Config.ResolvedProps
          result = resolveConfig exampleRequiredConfig input

          expected =
            invalid
              ( [ { error: NotFound
                  , path: "screens.someScreen.states.someScreenState.primaryButton.states.enabled.margin"
                  , reference: "some.weird.non.existing.path"
                  }
                , { error: NotFound, path: "screens.someScreen.states.someScreenState.primaryButton.margin", reference: "some.weird.non.existing.path" }
                , { error: NotFound, path: "screens.someScreen.states.someScreenState.gridItem.states.hidden.primaryButton.states.enabled.margin", reference: "some.weird.non.existing.path" }
                , { error: NotFound, path: "screens.someScreen.states.someScreenState.gridItem.states.hidden.primaryButton.margin", reference: "some.weird.non.existing.path" }
                , { error: (WrongType { actual: (RuntimeRecord (fromFoldable [ (Tuple "margin" RuntimeInt), (Tuple "padding" RuntimeInt) ])), expected: RuntimeInt }), path: "screens.someScreen.states.someScreenState.gridItem.states.hidden.padding", reference: "themeConfig" }
                , { error: NotFound, path: "screens.someScreen.states.someScreenState.gridItem.primaryButton.states.enabled.margin", reference: "some.weird.non.existing.path" }
                , { error: NotFound, path: "screens.someScreen.states.someScreenState.gridItem.primaryButton.margin", reference: "some.weird.non.existing.path" }
                , { error: (WrongType { actual: (RuntimeRecord (fromFoldable [ (Tuple "margin" RuntimeInt), (Tuple "padding" RuntimeInt) ])), expected: RuntimeInt }), path: "screens.someScreen.states.someScreenState.gridItem.padding", reference: "themeConfig" }
                , { error: NotFound, path: "screens.someScreen.primaryButton.states.enabled.margin", reference: "some.weird.non.existing.path" }
                , { error: NotFound, path: "screens.someScreen.primaryButton.margin", reference: "some.weird.non.existing.path" }
                , { error: NotFound, path: "screens.someScreen.gridItem.states.hidden.primaryButton.states.enabled.margin", reference: "some.weird.non.existing.path" }
                , { error: NotFound, path: "screens.someScreen.gridItem.states.hidden.primaryButton.margin", reference: "some.weird.non.existing.path" }
                , { error: (WrongType { actual: (RuntimeRecord (fromFoldable [ (Tuple "margin" RuntimeInt), (Tuple "padding" RuntimeInt) ])), expected: RuntimeInt }), path: "screens.someScreen.gridItem.states.hidden.padding", reference: "themeConfig" }
                , { error: NotFound, path: "screens.someScreen.gridItem.primaryButton.states.enabled.margin", reference: "some.weird.non.existing.path" }
                , { error: NotFound, path: "screens.someScreen.gridItem.primaryButton.margin", reference: "some.weird.non.existing.path" }
                , { error: (WrongType { actual: (RuntimeRecord (fromFoldable [ (Tuple "margin" RuntimeInt), (Tuple "padding" RuntimeInt) ])), expected: RuntimeInt }), path: "screens.someScreen.gridItem.padding", reference: "themeConfig" }
                , { error: NotFound, path: "containers.gridItem.states.hidden.primaryButton.states.enabled.margin", reference: "some.weird.non.existing.path" }
                , { error: NotFound, path: "containers.gridItem.states.hidden.primaryButton.margin", reference: "some.weird.non.existing.path" }
                , { error: (WrongType { actual: (RuntimeRecord (fromFoldable [ (Tuple "margin" RuntimeInt), (Tuple "padding" RuntimeInt) ])), expected: RuntimeInt }), path: "containers.gridItem.states.hidden.padding", reference: "themeConfig" }
                , { error: NotFound, path: "containers.gridItem.primaryButton.states.enabled.margin", reference: "some.weird.non.existing.path" }
                , { error: NotFound, path: "containers.gridItem.primaryButton.margin", reference: "some.weird.non.existing.path" }
                , { error: (WrongType { actual: (RuntimeRecord (fromFoldable [ (Tuple "margin" RuntimeInt), (Tuple "padding" RuntimeInt) ])), expected: RuntimeInt }), path: "containers.gridItem.padding", reference: "themeConfig" }
                , { error: NotFound, path: "components.primaryButton.states.enabled.margin", reference: "some.weird.non.existing.path" }
                , { error: NotFound, path: "components.primaryButton.margin", reference: "some.weird.non.existing.path" }
                ]
              )
        result `shouldEqual` expected
