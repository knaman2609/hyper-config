module Test.Extra.Heterogeneous.ForAll (spec) where

import Test.Prelude
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, notNull, toMaybe)
import Data.SubRecord (SubRecord, mkSubRecord)
import Extra.Data.Nullable (unsafeFromNullable)
import Extra.Heterogeneous.ForAll (ForAllMapping(..), ForAllMappingF(..), ForAllMappingN(..), ForAllMappingUnF(..))
import Global.Unsafe (unsafeStringify)
import Heterogeneous.Mapping (hmap)
import Test.Spec.Assertions (shouldEqual)

type S
  = ( int :: Int, number :: Number )

type NullableS
  = ( int :: Nullable Int, number :: Nullable Number )

spec :: Spec Unit
spec = do
  describe "forAllMapping" do
    it "should work with empty" do
      let
        result = hmap (ForAllMapping unsafeStringify) {}

        expected = {}
      result `shouldEqual` expected
    it "should map over different types at the same time" do
      let
        result = hmap (ForAllMapping unsafeStringify) { int: 123, number: 124.0 }

        expected = { int: "123", number: "124" }
      result `shouldEqual` expected
    it "should map over subrecord with different types at the same time" do
      let
        input :: SubRecord S
        input = mkSubRecord { int: 123, number: 124.0 }

        result = hmap (ForAllMapping unsafeStringify) input

        expected = mkSubRecord { int: "123", number: "124" }
      result `shouldEqual` expected
  describe "forAllMappingF" do
    it "should work with empty" do
      let
        result = hmap (ForAllMappingF notNull) {}

        expected = {}
      result `shouldEqual` expected
    it "should map over different types at the same time" do
      let
        result = hmap (ForAllMappingF notNull) { int: 123, number: 123.0 }

        expected = { int: notNull 123, number: notNull 123.0 }
      result `shouldEqual` expected
    it "should map over subrecord with different types at the same time" do
      let
        input :: SubRecord S
        input = mkSubRecord { int: 123, number: 123.0 }

        result = hmap (ForAllMappingF notNull) input

        expected = mkSubRecord { int: notNull 123, number: notNull 123.0 }
      result `shouldEqual` expected
  describe "forAllMappingUnF" do
    it "should work with empty" do
      let
        result = hmap (ForAllMappingUnF unsafeFromNullable) {}

        expected = {}
      result `shouldEqual` expected
    it "should map over different types at the same time" do
      let
        result = hmap (ForAllMappingUnF unsafeFromNullable) { int: notNull 123, number: notNull 123.0 }

        expected = { int: 123, number: 123.0 }
      result `shouldEqual` expected
    it "should map over subrecord with different types at the same time" do
      let
        input :: SubRecord NullableS
        input = mkSubRecord { int: notNull 123, number: notNull 123.0 }

        result = hmap (ForAllMappingUnF unsafeFromNullable) input

        expected = mkSubRecord { int: 123, number: 123.0 }
      result `shouldEqual` expected
  describe "forAllMappingN" do
    it "should work with empty" do
      let
        result = hmap (ForAllMappingN toMaybe) {}

        expected = {}
      result `shouldEqual` expected
    it "should map over different types at the same time" do
      let
        result = hmap (ForAllMappingN toMaybe) { int: notNull 123, number: notNull 123.0 }

        expected = { int: Just 123, number: Just 123.0 }
      result `shouldEqual` expected
    it "should map over subrecord with different types at the same time" do
      let
        input :: SubRecord NullableS
        input = mkSubRecord { int: notNull 123, number: notNull 123.0 }

        result = hmap (ForAllMappingN toMaybe) input

        expected = mkSubRecord { int: Just 123, number: Just 123.0 }
      result `shouldEqual` expected
