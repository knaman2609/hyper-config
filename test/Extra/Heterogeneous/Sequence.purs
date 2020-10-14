module Test.Extra.Heterogeneous.Sequence (spec) where

import Test.Prelude
import Data.Maybe (Maybe(..))
import Data.SubRecord (SubRecord, mkSubRecord)
import Extra.Heterogeneous.Sequence (hsequenceRecursive)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "hsequenceRecursiveRecord" do
    it "should sequence empty" do
      let
        result = hsequenceRecursive {}

        expected = Just {}
      result `shouldEqual` expected
    it "should sequence non matching" do
      let
        result = hsequenceRecursive { foo: "123" }

        expected = Just { foo: "123" }
      result `shouldEqual` expected
    it "should sequence" do
      let
        result = hsequenceRecursive { foo: Just "123" }

        expected = Just { foo: "123" }
      result `shouldEqual` expected
    it "should sequence nothing" do
      let
        result = hsequenceRecursive { foo: Nothing }

        expected = Nothing :: Maybe { foo :: Int }
      result `shouldEqual` expected
    it "should sequence inner" do
      let
        result = hsequenceRecursive { foo: { bar: Just 123 } }

        expected = Just { foo: { bar: 123 } }
      result `shouldEqual` expected
    it "should sequence mix of record and subrecord" do
      let
        input :: { foo :: SubRecord ( bar :: Maybe Int ) }
        input = { foo: mkSubRecord { bar: Just 123 } }

        result = hsequenceRecursive input

        expected = Just { foo: mkSubRecord { bar: 123 } }
      result `shouldEqual` expected
  describe "hsequenceRecursiveSubRecord" do
    it "should sequence empty subrecord" do
      let
        input :: SubRecord ()
        input = mkSubRecord {}

        result = hsequenceRecursive input

        expected = Just $ mkSubRecord {}
      result `shouldEqual` expected
    it "should sequence non matching subrecord" do
      let
        input :: SubRecord ( foo :: String )
        input = mkSubRecord { foo: "123" }

        result = hsequenceRecursive input

        expected = Just $ mkSubRecord { foo: "123" }
      result `shouldEqual` expected
    it "should sequence subrecord" do
      let
        input :: SubRecord ( foo :: Maybe String )
        input = mkSubRecord { foo: Just "123" }

        result = hsequenceRecursive input

        expected = Just $ mkSubRecord { foo: "123" }
      result `shouldEqual` expected
    it "should sequence nothing subrecord" do
      let
        input :: SubRecord ( foo :: Maybe Int )
        input = mkSubRecord { foo: Nothing }

        result = hsequenceRecursive input

        expected = Nothing
      result `shouldEqual` expected
    it "should sequence inner subrecord" do
      let
        input :: SubRecord ( foo :: SubRecord ( bar :: Maybe Int ) )
        input = mkSubRecord { foo: mkSubRecord { bar: Just 123 } }

        result = hsequenceRecursive input

        expected = Just $ mkSubRecord { foo: mkSubRecord { bar: 123 } }
      result `shouldEqual` expected
    it "should sequence mix of subrecord and record" do
      let
        input :: SubRecord ( foo :: Record ( bar :: Maybe Int ) )
        input = mkSubRecord { foo: { bar: Just 123 } }

        result = hsequenceRecursive input

        expected = Just $ mkSubRecord { foo: { bar: 123 } }
      result `shouldEqual` expected
