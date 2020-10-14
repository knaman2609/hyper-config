module Test.Data.SubRecord.Heterogeneous (spec) where

import Test.Prelude
import Data.Nullable (null)
import Data.SubRecord (SubRecord, mkSubRecord)
import Data.SubRecord.Heterogeneous (hfillFromBiggerRecursive, hfillFromSmallerRecursive, mkSubRecordRecursive)
import HyperWidgetConfig.Config as Config
import HyperWidgetConfig.Config.Components.GridItem as GridItem
import HyperWidgetConfig.Config.Components.PrimaryButton as PrimaryButton
import HyperWidgetConfig.Reference (Reference(..))
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "hfillFromSmallerRecursive" do
    describe "real-case compile-time checks" do
      itCompileTimeCheck "should work for primary button props" do
        let
          env :: {}
          env = any

          input :: PrimaryButton.Props
          input = any

          result = hfillFromSmallerRecursive env input

          expected :: PrimaryButton.Props
          expected = any
        pure unit
    describe "hfillFromSmallerRecursiveSS" do
      it "should work for empty" do
        let
          env :: SubRecord ()
          env = mkSubRecord {}

          input :: SubRecord ()
          input = mkSubRecord {}

          result = hfillFromSmallerRecursive env input

          expected = mkSubRecord {}
        result `shouldEqual` expected
      it "should work for left empty" do
        let
          env :: SubRecord ()
          env = mkSubRecord {}

          input :: SubRecord ( foo :: String )
          input = mkSubRecord { foo: "hello world" }

          result = hfillFromSmallerRecursive env input

          expected = mkSubRecord { foo: "hello world" }
        result `shouldEqual` expected
      it "should work for right empty" do
        let
          env :: SubRecord ( foo :: String )
          env = mkSubRecord { foo: "hello world" }

          input :: SubRecord ()
          input = mkSubRecord {}

          result = hfillFromSmallerRecursive env input

          expected = mkSubRecord {}
        result `shouldEqual` expected
      it "should work for flat" do
        let
          env :: SubRecord ( foo :: String, bar :: String )
          env = mkSubRecord { foo: "i should be present", bar: "i should not override" }

          input :: SubRecord ( foo :: String, bar :: String )
          input = mkSubRecord { bar: "do not override me!" }

          result = hfillFromSmallerRecursive env input

          expected = mkSubRecord { foo: "i should be present", bar: "do not override me!" }
        result `shouldEqual` expected
      it "should work for 1-level deep" do
        let
          env :: SubRecord ( inner :: SubRecord ( foo :: String ) )
          env = mkSubRecordRecursive { inner: { foo: "i should be present" } }

          input :: SubRecord ( inner :: SubRecord ( foo :: String ) )
          input = mkSubRecord {}

          result = hfillFromSmallerRecursive env input

          expected = mkSubRecordRecursive { inner: { foo: "i should be present" } }
        result `shouldEqual` expected
      it "should work for 2-level deep" do
        let
          env :: SubRecord ( inner :: SubRecord ( inner2 :: SubRecord ( foo :: String ) ) )
          env = mkSubRecordRecursive { inner: { inner2: { foo: "i should be present" } } }

          input :: SubRecord ( inner :: SubRecord ( inner2 :: SubRecord ( foo :: String ) ) )
          input = mkSubRecord {}

          result = hfillFromSmallerRecursive env input

          expected = mkSubRecordRecursive { inner: { inner2: { foo: "i should be present" } } }
        result `shouldEqual` expected
    describe "hfillFromSmallerRecursiveRR" do
      it "should work for empty" do
        let
          env = {}

          input = {}

          result = hfillFromSmallerRecursive env input

          expected = {}
        result `shouldEqual` expected
      it "should work for left empty" do
        let
          env = {}

          input = { foo: "hello world" }

          result = hfillFromSmallerRecursive env input

          expected = { foo: "hello world" }
        result `shouldEqual` expected
      it "should work for right empty" do
        let
          env = { foo: "hello world" }

          input = {}

          result = hfillFromSmallerRecursive env input

          expected = {}
        result `shouldEqual` expected
      it "should work for flat" do
        let
          env = { foo: "i should not be present", bar: "i should not override" }

          input = { bar: "do not override me!" }

          result = hfillFromSmallerRecursive env input

          expected = { bar: "do not override me!" }
        result `shouldEqual` expected
    describe "hfillFromSmallerRecursiveRS" do
      it "should work for empty" do
        let
          env = {}

          input :: SubRecord ()
          input = mkSubRecord {}

          result = hfillFromSmallerRecursive env input

          expected = mkSubRecord {}
        result `shouldEqual` expected
      it "should work for left empty" do
        let
          env = {}

          input :: SubRecord ( foo :: String )
          input = mkSubRecord { foo: "hello world" }

          result = hfillFromSmallerRecursive env input

          expected = mkSubRecord { foo: "hello world" }
        result `shouldEqual` expected
      it "should work for right empty" do
        let
          env = { foo: "hello world" }

          input :: SubRecord ()
          input = mkSubRecord {}

          result = hfillFromSmallerRecursive env input

          expected = mkSubRecord {}
        result `shouldEqual` expected
      it "should work for flat" do
        let
          env = { foo: "i should be present", bar: "i should not override" }

          input :: SubRecord ( foo :: String, bar :: String )
          input = mkSubRecord { bar: "do not override me!" }

          result = hfillFromSmallerRecursive env input

          expected = mkSubRecord { foo: "i should be present", bar: "do not override me!" }
        result `shouldEqual` expected
      it "should work for 1-level deep" do
        let
          env = { inner: { foo: "i should be present" } }

          input :: SubRecord ( inner :: SubRecord ( foo :: String ) )
          input = mkSubRecord {}

          result = hfillFromSmallerRecursive env input

          expected = mkSubRecordRecursive { inner: { foo: "i should be present" } }
        result `shouldEqual` expected
      it "should work for 2-level deep" do
        let
          env = { inner: { inner2: { foo: "i should be present" } } }

          input :: SubRecord ( inner :: SubRecord ( inner2 :: SubRecord ( foo :: String ) ) )
          input = mkSubRecord {}

          result = hfillFromSmallerRecursive env input

          expected = mkSubRecordRecursive { inner: { inner2: { foo: "i should be present" } } }
        result `shouldEqual` expected
  describe "hfillFromBiggerRecursive" do
    describe "hfillFromBiggerRecursiveSRandSR" do
      it "should work for empty" do
        let
          env :: SubRecord ()
          env = mkSubRecord {}

          input :: SubRecord ()
          input = mkSubRecord {}

          result = hfillFromBiggerRecursive env input

          expected = mkSubRecord {}
        result `shouldEqual` expected
      it "should work for right empty" do
        let
          env :: SubRecord ( foo :: String )
          env = mkSubRecord { foo: "hello world" }

          input :: SubRecord ()
          input = mkSubRecord {}

          result = hfillFromBiggerRecursive env input

          expected = mkSubRecord {}
        result `shouldEqual` expected
      it "should work for flat" do
        let
          env :: SubRecord ( foo :: String, bar :: String )
          env = mkSubRecord { foo: "i should be present", bar: "i should not override" }

          input :: SubRecord ( foo :: String, bar :: String )
          input = mkSubRecord { bar: "do not override me!" }

          result = hfillFromBiggerRecursive env input

          expected = mkSubRecord { foo: "i should be present", bar: "do not override me!" }
        result `shouldEqual` expected
      it "should work for 1-level deep" do
        let
          env :: SubRecord ( inner :: SubRecord ( foo :: String ) )
          env = mkSubRecordRecursive { inner: { foo: "i should be present" } }

          input :: SubRecord ( inner :: SubRecord ( foo :: String ) )
          input = mkSubRecord {}

          result = hfillFromBiggerRecursive env input

          expected = mkSubRecordRecursive { inner: { foo: "i should be present" } }
        result `shouldEqual` expected
      it "should work for 2-level deep" do
        let
          env :: SubRecord ( inner :: SubRecord ( inner2 :: SubRecord ( foo :: String ) ) )
          env = mkSubRecordRecursive { inner: { inner2: { foo: "i should be present" } } }

          input :: SubRecord ( inner :: SubRecord ( inner2 :: SubRecord ( foo :: String ) ) )
          input = mkSubRecord {}

          result = hfillFromBiggerRecursive env input

          expected = mkSubRecordRecursive { inner: { inner2: { foo: "i should be present" } } }
        result `shouldEqual` expected
    describe "hfillFromBiggerRecursiveRandSR" do
      it "should work for empty" do
        let
          env = {}

          input :: SubRecord ()
          input = mkSubRecord {}

          result = hfillFromBiggerRecursive env input

          expected = {}
        result `shouldEqual` expected
      it "should work for right empty" do
        let
          env = { foo: "hello world" }

          input :: SubRecord ()
          input = mkSubRecord {}

          result = hfillFromBiggerRecursive env input

          expected = {}
        result `shouldEqual` expected
      it "should work for flat" do
        let
          env = { foo: "i should be present", bar: "i should not override" }

          input :: SubRecord ( foo :: String, bar :: String )
          input = mkSubRecord { bar: "do not override me!" }

          result = hfillFromBiggerRecursive env input

          expected = { foo: "i should be present", bar: "do not override me!" }
        result `shouldEqual` expected
      it "should work for 1-level deep" do
        let
          env = { inner: { foo: "i should be present" } }

          input :: SubRecord ( inner :: SubRecord ( foo :: String ) )
          input = mkSubRecord {}

          result = hfillFromBiggerRecursive env input

          expected = { inner: { foo: "i should be present" } }
        result `shouldEqual` expected
      it "should work for 2-level deep" do
        let
          env = { inner: { inner2: { foo: "i should be present" } } }

          input :: SubRecord ( inner :: SubRecord ( inner2 :: SubRecord ( foo :: String ) ) )
          input = mkSubRecord {}

          result = hfillFromBiggerRecursive env input

          expected = { inner: { inner2: { foo: "i should be present" } } }
        result `shouldEqual` expected
      it "should work for lesser record" do
        let
          env = { component: "whatever" }

          input :: SubRecord ( component :: String, foo :: Int )
          input = mkSubRecord {}

          result = hfillFromBiggerRecursive env input

          expected = { component: "whatever", foo: null }
        result `shouldEqual` expected
      it "should work for props mini-example" do
        let
          env =
            { containers:
                { gridItem:
                    { padding: Literal 123
                    , primaryButton:
                        { component: "qwer"
                        }
                    , states: mkSubRecord {} :: GridItem.StatesF GridItem.State SubRecord
                    }
                }
            }

          input ::
            SubRecord
              ( containers ::
                  SubRecord
                    ( gridItem ::
                        SubRecord
                          ( padding :: Reference Int
                          , primaryButton ::
                              { component :: String
                              , margin :: Reference Int
                              , states :: PrimaryButton.StatesF PrimaryButton.State SubRecord
                              }
                          , states :: GridItem.StatesF GridItem.State SubRecord
                          )
                    )
              )
          input = mkSubRecord {}

          result = hfillFromBiggerRecursive env input

          expected =
            { containers:
                { gridItem:
                    { padding: (Literal 123)
                    , primaryButton:
                        { component: "qwer"
                        , margin: null
                        , states: null
                        }
                    , states: mkSubRecord {}
                    }
                }
            }
        result `shouldEqual` expected
  describe "mkSubRecordRecursive" do
    itCompileTimeCheck "should compile-time check" do
      let
        correct ::
          SubRecord
            ( bar :: String
            , foo :: Int
            , qwer :: String
            , inner :: SubRecord ( hello :: String, another :: Int )
            )
        correct =
          mkSubRecordRecursive
            ( any ::
                { foo :: Int
                , qwer :: String
                , inner :: { hello :: String }
                }
            )
      pure unit
    it "should convert example config" do
      let
        result :: Config.Props
        result =
          mkSubRecordRecursive
            { themeConfig:
                { padding: 123
                }
            , components:
                { primaryButton:
                    { margin: Literal 444
                    }
                }
            }
      pure unit
    it "should convert example config 2" do
      let
        result :: Config.Props
        result =
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
      pure unit
