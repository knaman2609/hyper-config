module Test.Extra.Heterogeneous (spec) where

import Test.Prelude
import Data.SubRecord (SubRecord, mkSubRecord)
import Data.SubRecord.Heterogeneous (mkSubRecordRecursive)
import Data.Symbol (class IsSymbol, SProxy, reflectSymbol)
import Extra.Data.Lens.Indexed (ReversedSegments, toPath)
import Extra.Heterogeneous (MappingWithIndex(..), mkRecursiveMappingWithIndexAggregated)
import Extra.Heterogeneous.Loosely (LooselyMapping(..))
import Extra.Heterogeneous.Recursive (RecursiveMapping(..))
import Heterogeneous.Mapping (hmap, hmapWithIndex)
import Test.Spec.Assertions (shouldEqual)

intToString :: Int -> String
intToString = show

spec :: Spec Unit
spec = do
  describe "MappingWithIndex" do
    let
      f :: forall prop. IsSymbol prop => SProxy prop -> Int -> String
      f prop value = reflectSymbol prop <> ": " <> show value

      m = MappingWithIndex f
    it "should work" do
      let
        result = hmapWithIndex m { foo: 123 }

        expected = { foo: "foo: 123" }
      result `shouldEqual` expected
  describe "mkRecursiveMappingWithIndexAggregated" do
    let
      f :: ReversedSegments -> Int -> String
      f segments value = toPath segments <> ": " <> show value

      m = mkRecursiveMappingWithIndexAggregated f
    it "should work with 2-level record" do
      let
        result = hmapWithIndex m { int: 123, inner: { int2: 321 } }

        expected = { int: "int: 123", inner: { int2: "inner.int2: 321" } }
      result `shouldEqual` expected
  describe "RecursiveMapping + LooselyMapping (records)" do
    let
      m = RecursiveMapping $ LooselyMapping intToString
    it "should work with empty record" do
      let
        result = hmap m {}
      result `shouldEqual` {}
    it "should work with 1-level record" do
      let
        result = hmap m { other: 123.0, inner1: { int: 123 } }
      result `shouldEqual` { other: 123.0, inner1: { int: "123" } }
    it "should work with 2-level record" do
      let
        result = hmap m { inner1: { inner2: { int: 123 } } }
      result `shouldEqual` { inner1: { inner2: { int: "123" } } }
    it "should work with mix of record and subrecord" do
      let
        input :: Record ( inner1 :: SubRecord ( inner2 :: Record ( int :: Int ) ) )
        input = { inner1: mkSubRecord { inner2: { int: 123 } } }

        result = hmap m input
      result `shouldEqual` { inner1: mkSubRecord { inner2: { int: "123" } } }
  describe "RecursiveMapping + LooselyMapping (subrecords)" do
    let
      m = RecursiveMapping $ LooselyMapping intToString
    it "should work with empty subrecord" do
      let
        input :: SubRecord ()
        input = mkSubRecord {}

        result = hmap m input
      result `shouldEqual` mkSubRecord {}
    it "should work with 0-level subrecord" do
      let
        input :: SubRecord ( int :: Int )
        input = mkSubRecord { int: 123 }

        result = hmap m input
      result `shouldEqual` mkSubRecord { int: "123" }
    it "should loosely work with 0-level subrecord" do
      let
        input :: SubRecord ( int :: Int, other :: Number )
        input = mkSubRecord { int: 123, other: 123.0 }

        result = hmap m input
      result `shouldEqual` mkSubRecord { int: "123", other: 123.0 }
    it "should work with 1-level subrecord" do
      let
        input :: SubRecord ( other :: Number, inner1 :: SubRecord ( int :: Int ) )
        input = mkSubRecordRecursive { other: 123.0, inner1: { int: 123 } }

        result = hmap m input
      result `shouldEqual` mkSubRecordRecursive { other: 123.0, inner1: { int: "123" } }
    it "should work with 2-level subrecord" do
      let
        input :: SubRecord ( inner1 :: SubRecord ( inner2 :: SubRecord ( int :: Int ) ) )
        input = mkSubRecordRecursive { inner1: { inner2: { int: 123 } } }

        result = hmap m input
      result `shouldEqual` mkSubRecordRecursive { inner1: { inner2: { int: "123" } } }
