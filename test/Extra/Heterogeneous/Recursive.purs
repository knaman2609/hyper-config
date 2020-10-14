module Test.Extra.Heterogeneous.Recursive (spec) where

import Test.Prelude
import Data.List (List(..))
import Data.Nullable (notNull)
import Data.SubRecord (SubRecord, mkSubRecord)
import Data.SubRecord.Heterogeneous (mkSubRecordRecursive)
import Data.Symbol (class IsSymbol, SProxy, reflectSymbol)
import Extra.Data.Lens.Indexed (ReversedSegments(..), addSegment, toPath)
import Extra.Heterogeneous (MappingWithIndex(..))
import Extra.Heterogeneous.ForAll (ForAllMappingF(..))
import Extra.Heterogeneous.Recursive (RecursiveMapping(..), RecursiveMappingWithIndex(..), TotalMapping(..))
import Heterogeneous.Mapping (hmap, hmapWithIndex)
import Test.Spec.Assertions (shouldEqual)

intToString :: Int -> String
intToString = show

spec :: Spec Unit
spec = do
  describe "RecursiveMapping" do
    it "should work with empty record" do
      let
        result = hmap (RecursiveMapping intToString) {}
      result `shouldEqual` {}
    it "should work with 2-level record" do
      let
        result = hmap (RecursiveMapping intToString) { inner1: { int: 123 } }
      result `shouldEqual` { inner1: { int: "123" } }
    it "should work with 3-level record" do
      let
        result = hmap (RecursiveMapping intToString) { inner1: { inner2: { int: 123 } } }
      result `shouldEqual` { inner1: { inner2: { int: "123" } } }
    it "should work with empty subrecord" do
      let
        input :: SubRecord ()
        input = mkSubRecord {}

        result = hmap (RecursiveMapping intToString) input
      result `shouldEqual` mkSubRecord {}
    it "should work with 2-level subrecord" do
      let
        input :: SubRecord ( inner1 :: SubRecord ( int :: Int ) )
        input = mkSubRecordRecursive { inner1: { int: 123 } }

        result = hmap (RecursiveMapping intToString) input
      result `shouldEqual` mkSubRecordRecursive { inner1: { int: "123" } }
    it "should work with 3-level subrecord" do
      let
        input :: SubRecord ( inner1 :: SubRecord ( inner2 :: SubRecord ( int :: Int ) ) )
        input = mkSubRecordRecursive { inner1: { inner2: { int: 123 } } }

        result = hmap (RecursiveMapping intToString) input
      result `shouldEqual` mkSubRecordRecursive { inner1: { inner2: { int: "123" } } }
    it "should work with mix of record and subrecord" do
      let
        input :: Record ( inner1 :: SubRecord ( inner2 :: Record ( int :: Int ) ) )
        input = { inner1: mkSubRecord { inner2: { int: 123 } } }

        result = hmap (RecursiveMapping intToString) input
      result `shouldEqual` { inner1: mkSubRecord { inner2: { int: "123" } } }
  describe "RecursiveMappingWithIndex (records)" do
    let
      mkF' :: ReversedSegments -> MappingWithIndex (Int -> String)
      mkF' segments = MappingWithIndex f
        where
        f :: forall prop. IsSymbol prop => SProxy prop -> Int -> String
        f prop value = toPath segments' <> ": " <> show value
          where
          segment = reflectSymbol prop

          segments' = addSegment segment segments

      mkF :: forall prop. IsSymbol prop => ReversedSegments -> SProxy prop -> RecursiveMappingWithIndex (MappingWithIndex (Int -> String))
      mkF segments prop = RecursiveMappingWithIndex { mkF: mkF segments', f: mkF' segments' }
        where
        segment = reflectSymbol prop

        segments' = addSegment segment segments

      m = RecursiveMappingWithIndex { mkF: mkF (ReversedSegments Nil), f: mkF' (ReversedSegments Nil) }
    it "should work with empty record" do
      let
        result = hmapWithIndex m {}

        expected = {}
      result `shouldEqual` {}
    it "should work with 1-level record" do
      let
        result = hmapWithIndex m { foo: 123 }

        expected = { foo: "foo: 123" }
      result `shouldEqual` expected
    it "should work with 2-level record" do
      let
        result = hmapWithIndex m { inner: { foo: 123 } }

        expected = { inner: { foo: "inner.foo: 123" } }
      result `shouldEqual` expected
    it "should work with record+subrecord" do
      let
        input :: { inner :: SubRecord ( foo :: Int ) }
        input = { inner: mkSubRecord { foo: 123 } }

        result = hmapWithIndex m input

        expected = { inner: mkSubRecord { foo: "inner.foo: 123" } }
      result `shouldEqual` expected
    it "should work with 3-level record" do
      let
        result = hmapWithIndex m { inner: { inner2: { foo: 123 } } }

        expected = { inner: { inner2: { foo: "inner.inner2.foo: 123" } } }
      result `shouldEqual` expected
    it "should work with record+subrecord+record" do
      let
        input :: { inner :: SubRecord ( inner2 :: { foo :: Int } ) }
        input = { inner: mkSubRecord { inner2: { foo: 123 } } }

        result = hmapWithIndex m input

        expected = { inner: mkSubRecord { inner2: { foo: "inner.inner2.foo: 123" } } }
      result `shouldEqual` expected
  describe "RecursiveMappingWithIndex (subrecords)" do
    let
      mkF' :: ReversedSegments -> MappingWithIndex (Int -> String)
      mkF' segments = MappingWithIndex f
        where
        f :: forall prop. IsSymbol prop => SProxy prop -> Int -> String
        f prop value = toPath segments' <> ": " <> show value
          where
          segment = reflectSymbol prop

          segments' = addSegment segment segments

      mkF :: forall prop. IsSymbol prop => ReversedSegments -> SProxy prop -> RecursiveMappingWithIndex (MappingWithIndex (Int -> String))
      mkF segments prop = RecursiveMappingWithIndex { mkF: mkF segments', f: mkF' segments' }
        where
        segment = reflectSymbol prop

        segments' = addSegment segment segments

      m = RecursiveMappingWithIndex { mkF: mkF (ReversedSegments Nil), f: mkF' (ReversedSegments Nil) }
    it "should work with empty subrecord" do
      let
        input :: SubRecord ()
        input = mkSubRecord {}

        result = hmapWithIndex m input

        expected = mkSubRecord {}
      result `shouldEqual` expected
    it "should work with 1-level subrecord" do
      let
        input :: SubRecord ( foo :: Int )
        input = mkSubRecord { foo: 123 }

        result = hmapWithIndex m input

        expected = mkSubRecord { foo: "foo: 123" }
      result `shouldEqual` expected
    it "should work with 2-level subrecord" do
      let
        input :: SubRecord ( inner :: SubRecord ( foo :: Int ) )
        input = mkSubRecord { inner: mkSubRecord { foo: 123 } }

        result = hmapWithIndex m input

        expected = mkSubRecord { inner: mkSubRecord { foo: "inner.foo: 123" } }
      result `shouldEqual` expected
    it "should work with subrecord+record" do
      let
        input :: SubRecord ( inner :: { foo :: Int } )
        input = mkSubRecord { inner: { foo: 123 } }

        result = hmapWithIndex m input

        expected = mkSubRecord { inner: { foo: "inner.foo: 123" } }
      result `shouldEqual` expected
    it "should work with 3-level subrecord" do
      let
        input :: SubRecord ( inner :: SubRecord ( inner2 :: SubRecord ( foo :: Int ) ) )
        input = mkSubRecord { inner: mkSubRecord { inner2: mkSubRecord { foo: 123 } } }

        result = hmapWithIndex m input

        expected = mkSubRecord { inner: mkSubRecord { inner2: mkSubRecord { foo: "inner.inner2.foo: 123" } } }
      result `shouldEqual` expected
    it "should work with subrecord+record+subrecord" do
      let
        input :: SubRecord ( inner :: { inner2 :: SubRecord ( foo :: Int ) } )
        input = mkSubRecord { inner: { inner2: mkSubRecord { foo: 123 } } }

        result = hmapWithIndex m input

        expected = mkSubRecord { inner: { inner2: mkSubRecord { foo: "inner.inner2.foo: 123" } } }
      result `shouldEqual` expected
  describe "TotalMapping (records)" do
    let
      -- have to use $ instead of <<< due to escaping type variables
      m = TotalMapping $ ForAllMappingF notNull
    it "should work with empty record" do
      let
        result = hmap m {}
      result `shouldEqual` {}
    it "should work with 1-level record" do
      let
        result = hmap m { int: 123 }
      result `shouldEqual` { int: notNull 123 }
    it "should work with 2-level record" do
      let
        result = hmap m { inner1: { int: 123 } }
      result `shouldEqual` { inner1: notNull { int: notNull 123 } }
    it "should work with 3-level record" do
      let
        result = hmap m { inner1: { inner2: { int: 123 } } }
      result `shouldEqual` { inner1: notNull { inner2: notNull { int: notNull 123 } } }
  describe "TotalMapping (subrecords)" do
    let
      -- have to use $ instead of <<< due to escaping type variables
      m = TotalMapping $ ForAllMappingF notNull
    it "should work with empty subrecord" do
      let
        input :: SubRecord ()
        input = mkSubRecord {}

        result = hmap m input
      result `shouldEqual` mkSubRecord {}
    it "should work with 1-level subrecord" do
      let
        input :: SubRecord ( int :: Int )
        input = mkSubRecord { int: 123 }

        result = hmap m input
      result `shouldEqual` mkSubRecord { int: notNull 123 }
    it "should work with 2-level subrecord" do
      let
        input :: SubRecord ( inner1 :: SubRecord ( int :: Int ) )
        input = mkSubRecord { inner1: mkSubRecord { int: 123 } }

        result = hmap m input
      result `shouldEqual` mkSubRecord { inner1: notNull $ mkSubRecord { int: notNull 123 } }
    it "should work with 3-level subrecord" do
      let
        input :: SubRecord ( inner1 :: SubRecord ( inner2 :: SubRecord ( int :: Int ) ) )
        input = mkSubRecord { inner1: mkSubRecord { inner2: mkSubRecord { int: 123 } } }

        result = hmap m input
      result `shouldEqual` mkSubRecord { inner1: notNull $ mkSubRecord { inner2: notNull $ mkSubRecord { int: notNull 123 } } }
    it "should work with mix of record and subrecord" do
      let
        input :: SubRecord ( inner1 :: Record ( inner2 :: SubRecord ( int :: Int ) ) )
        input = mkSubRecord { inner1: { inner2: mkSubRecord { int: 123 } } }

        result = hmap m input
      result `shouldEqual` mkSubRecord { inner1: notNull { inner2: notNull $ mkSubRecord { int: notNull 123 } } }
