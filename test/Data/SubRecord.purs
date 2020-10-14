module Test.Data.SubRecord (spec) where

import Test.Prelude
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, null, toMaybe)
import Data.SubRecord (SRtoR(..), SubRecord, hmapRtoSR, hmapSRtoR, hmapWithIndexRtoSR, hmapWithIndexSRtoR, mergeSubRecord, mkSubRecord)
import Data.SubRecord.Heterogeneous (mkSubRecordRecursive)
import Data.Symbol (class IsSymbol, SProxy, reflectSymbol)
import Heterogeneous.Folding (class Folding, class FoldingWithIndex, hfoldl, hfoldlWithIndex)
import Heterogeneous.Mapping (class MappingWithIndex, hmap, hmapWithIndex)
import Test.Spec.Assertions (shouldEqual)

type S
  = ( foo :: String
    , foo2 :: Int
    , bar :: String
    , inner :: SubRecord Inner
    )

type Small
  = ( int :: Int )

type Inner
  = ( fooInner :: Int
    )

type Extended
  = ( deleteMe :: String
    | S
    )

data IntToStringMappingWithIndex
  = IntToStringMappingWithIndex

instance intToStringMappingWithIndex ::
  IsSymbol prop =>
  MappingWithIndex IntToStringMappingWithIndex (SProxy prop) Int String where
  mappingWithIndex _ prop int = reflectSymbol prop <> ": " <> show int

data NullableIntToStringMappingWithIndex
  = NullableIntToStringMappingWithIndex

instance nullableIntToStringMappingWithIndex ::
  IsSymbol prop =>
  MappingWithIndex NullableIntToStringMappingWithIndex (SProxy prop) (Nullable Int) String where
  mappingWithIndex _ prop =
    toMaybe
      >>> case _ of
          Nothing -> show (null :: Nullable Int)
          Just int -> reflectSymbol prop <> ": " <> show int

data ShowPropsFolding
  = ShowPropsFolding

instance showPropsFolding ::
  Show a =>
  Folding ShowPropsFolding String a String where
  folding ShowPropsFolding str a = pre <> show a
    where
    pre
      | str == "" = ""
      | otherwise = str <> ", "

data ShowPropsFoldingWithIndex
  = ShowPropsFoldingWithIndex

instance showPropsFoldingWithIndex ::
  (Show a, IsSymbol sym) =>
  FoldingWithIndex ShowPropsFoldingWithIndex (SProxy sym) String a String where
  foldingWithIndex ShowPropsFoldingWithIndex prop str a = pre <> reflectSymbol prop <> ": " <> show a
    where
    pre
      | str == "" = ""
      | otherwise = str <> ", "

spec :: Spec Unit
spec = do
  describe "showSubRecord" do
    it "should show single prop" do
      let
        s :: SubRecord S
        s = mkSubRecord { foo: "fooValue" }
      show s `shouldEqual` """SubRecord ( foo: "fooValue" )"""
    it "should show multiple props" do
      let
        s :: SubRecord S
        s = mkSubRecord { foo: "fooValue", bar: "barValue" }
      show s `shouldEqual` """SubRecord ( bar: "barValue", foo: "fooValue" )"""
  describe "eqSubRecord" do
    it "should eq single prop" do
      let
        s1 :: SubRecord S
        s1 = mkSubRecord { foo: "fooValue" }

        s2 :: SubRecord S
        s2 = mkSubRecord { foo: "fooValue" }
      eq s1 s2 `shouldEqual` true
    it "should eq single prop when false" do
      let
        s1 :: SubRecord S
        s1 = mkSubRecord { foo: "fooValue" }

        s2 :: SubRecord S
        s2 = mkSubRecord { foo: "fooValue123" }
      eq s1 s2 `shouldEqual` false
    it "should false eq when additional props" do
      let
        s1 :: SubRecord S
        s1 = mkSubRecord { foo: "fooValue" }

        s2 :: SubRecord S
        s2 = mkSubRecord { foo: "fooValue", bar: "barValue" }
      eq s1 s2 `shouldEqual` false
  describe "monoidSubRecord" do
    it "should append non-overlapping props" do
      let
        s1 :: SubRecord S
        s1 = mkSubRecord { foo: "fooValue" }

        s2 :: SubRecord S
        s2 = mkSubRecord { bar: "barValue" }

        result = s1 <> s2

        expected :: SubRecord S
        expected = mkSubRecord { foo: "fooValue", bar: "barValue" }
      result `shouldEqual` expected
    it "should append overlapping props" do
      let
        s1 :: SubRecord S
        s1 = mkSubRecord { foo: "fooValue" }

        s2 :: SubRecord S
        s2 = mkSubRecord { foo: "barValue" }

        result = s1 <> s2

        expected :: SubRecord S
        expected = mkSubRecord { foo: "barValue" }
      result `shouldEqual` expected
    it "should append and not recurse" do
      let
        s1 :: SubRecord S
        s1 = mkSubRecordRecursive { inner: { fooInner: 123 } }

        s2 :: SubRecord S
        s2 = mkSubRecordRecursive { inner: {} }

        result = s1 <> s2

        expected :: SubRecord S
        expected = mkSubRecordRecursive { inner: {} } -- inner is overriden instead of appended
      result `shouldEqual` expected
  describe "mergeSubRecord" do
    it "should pass" do
      let
        s :: SubRecord S
        s = mkSubRecord { foo: "fooValue" }

        r :: Record S
        r =
          { foo: "fooValueBefore"
          , foo2: 123
          , bar: "barValueBefore"
          , inner: mkSubRecord {}
          }
      mergeSubRecord r s
        `shouldEqual`
          { foo: "fooValue"
          , foo2: 123
          , bar: "barValueBefore"
          , inner: mkSubRecord {}
          }
  describe "hmapSRtoSR" do
    it "should pass" do
      let
        input :: SubRecord Small
        input = mkSubRecord { int: 123 }

        result = hmap (show :: Int -> String) input

        expected = mkSubRecord { int: "123" }
      result `shouldEqual` expected
  describe "hmapRtoSR" do
    it "should pass" do
      let
        input = { int: 123 }

        result = hmapRtoSR (show :: Int -> String) input

        expected = mkSubRecord { int: "123" }
      result `shouldEqual` expected
  describe "hmapSRtoR" do
    it "should pass" do
      let
        input :: SubRecord Small
        input = mkSubRecord { int: 123 }

        result = hmapSRtoR (show :: Nullable Int -> String) input

        expected = { int: "123" }
      result `shouldEqual` expected
  describe "hmapSRtoRInst" do
    it "should pass" do
      let
        input :: SubRecord Small
        input = mkSubRecord { int: 123 }

        result = hmap (SRtoR (show :: Nullable Int -> String)) input

        expected = { int: "123" }
      result `shouldEqual` expected
  describe "hmapWithIndexSRtoSR" do
    it "should pass" do
      let
        input :: SubRecord Small
        input = mkSubRecord { int: 123 }

        result = hmapWithIndex IntToStringMappingWithIndex input

        expected = mkSubRecord { int: "int: 123" }
      result `shouldEqual` expected
  describe "hmapWithIndexRtoSR" do
    it "should pass" do
      let
        input = { int: 123 }

        result = hmapWithIndexRtoSR IntToStringMappingWithIndex input

        expected = mkSubRecord { int: "int: 123" }
      result `shouldEqual` expected
  describe "hmapWithIndexSRtoR" do
    it "should pass" do
      let
        input :: SubRecord ( int :: Int, null :: Int )
        input = mkSubRecord { int: 123 }

        result = hmapWithIndexSRtoR NullableIntToStringMappingWithIndex input

        expected = { int: "int: 123", null: "null" }
      result `shouldEqual` expected
  describe "hmapWithIndexSRtoRInst" do
    it "should pass" do
      let
        input :: SubRecord ( int :: Int, null :: Int )
        input = mkSubRecord { int: 123 }

        result = hmapWithIndex (SRtoR NullableIntToStringMappingWithIndex) input

        expected = { int: "int: 123", null: "null" }
      result `shouldEqual` expected
  describe "hfoldlSubRecord" do
    it "should pass" do
      let
        input :: SubRecord ( int :: Int, number :: Number, null :: Int )
        input = mkSubRecord { int: 123, number: 321.0 }

        result = hfoldl ShowPropsFolding "" input

        expected = "123, null, 321.0"
      result `shouldEqual` expected
  describe "hfoldlSubRecordWithIndex" do
    it "should pass" do
      let
        input :: SubRecord ( int :: Int, number :: Number, null :: Int )
        input = mkSubRecord { int: 123, number: 321.0 }

        result = hfoldlWithIndex ShowPropsFoldingWithIndex "" input

        expected = "int: 123, null: null, number: 321.0"
      result `shouldEqual` expected
