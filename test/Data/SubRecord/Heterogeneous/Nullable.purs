module Test.Data.SubRecord.Heterogeneous.Nullable (spec) where

import Test.Prelude
import Data.Nullable (Nullable, notNull, null)
import Data.SubRecord (SubRecord, mkSubRecord)
import Data.SubRecord.Heterogeneous (mkSubRecordRecursive)
import Data.SubRecord.Heterogeneous.Nullable (hfromNullable, htoNullable)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "htoNullable" do
    it "should work for empty" do
      let
        input :: SubRecord ()
        input = mkSubRecord {}

        result = htoNullable input

        expected = {}
      result `shouldEqual` expected
    it "should work for 1-level" do
      let
        input :: SubRecord ( foo :: String, bar :: Int )
        input = mkSubRecord { foo: "hello world" }

        result = htoNullable input

        expected = { foo: notNull "hello world", bar: null }
      result `shouldEqual` expected
    it "should not recurse for inner subrecord" do
      let
        input :: SubRecord ( inner :: SubRecord ( foo :: String ) )
        input = mkSubRecordRecursive { inner: { foo: "hello world" } }

        result = htoNullable input

        expected = { inner: notNull $ mkSubRecord { foo: "hello world" } }
      result `shouldEqual` expected
    it "should not recurse for inner record" do
      let
        input :: SubRecord ( inner :: Record ( foo :: String ) )
        input = mkSubRecord { inner: { foo: "hello world" } }

        result = htoNullable input

        expected = { inner: notNull $ { foo: "hello world" } }
      result `shouldEqual` expected
  describe "hfromNullable" do
    it "should work for empty" do
      let
        input = {}

        result = hfromNullable input

        expected = mkSubRecord {}
      result `shouldEqual` expected
    it "should work for 1-level" do
      let
        input = { foo: notNull "hello world", bar: (null :: Nullable Int) }

        result = hfromNullable input

        expected = mkSubRecord { foo: "hello world" }
      result `shouldEqual` expected
    it "should not recurse for inner subrecord" do
      let
        input = { inner: notNull $ (mkSubRecord { foo: "hello world" } :: SubRecord ( foo :: String )) }

        result = hfromNullable input

        expected = mkSubRecordRecursive { inner: { foo: "hello world" } }
      result `shouldEqual` expected
    it "should not recurse for inner record" do
      let
        input = { inner: notNull { foo: "hello world" } }

        result = hfromNullable input

        expected = mkSubRecord { inner: { foo: "hello world" } }
      result `shouldEqual` expected
