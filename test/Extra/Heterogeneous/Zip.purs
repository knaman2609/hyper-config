module Test.Extra.Heterogeneous.Zip (spec) where

import Test.Prelude
import Data.Nullable (Nullable, notNull, null)
import Data.SubRecord (SubRecord, mkSubRecord)
import Data.Tuple (Tuple(..))
import Extra.Heterogeneous.Zip (hzipRight)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "hzipRight" do
    describe "hzipRightRandR" do
      it "should work with empty" do
        let
          left = {}

          right = {}

          result = hzipRight left right

          expected = {}
        result `shouldEqual` expected
      it "should work with empty left" do
        let
          left = {}

          right = { foo: "hello world" }

          result = hzipRight left right

          expected = { foo: Tuple (null :: Nullable String) "hello world" }
        result `shouldEqual` expected
      it "should work with empty right" do
        let
          left = { foo: "hello world" }

          right = {}

          result = hzipRight left right

          expected = {}
        result `shouldEqual` expected
      it "should work with same props" do
        let
          left = { foo: "left" }

          right = { foo: "right" }

          result = hzipRight left right

          expected = { foo: Tuple "left" "right" }
        result `shouldEqual` expected
      it "should work with same two props" do
        let
          left = { bar: "barl", foo: "left" }

          right = { bar: "barr", foo: "right" }

          result = hzipRight left right

          expected = { bar: Tuple "barl" "barr", foo: Tuple "left" "right" }
        result `shouldEqual` expected
      it "should work with diff props" do
        let
          left = { foo: 123 }

          right = { foo: "right" }

          result = hzipRight left right

          expected = { foo: Tuple 123 "right" }
        result `shouldEqual` expected
    describe "hzipRightSRandSR" do
      it "should work with empty" do
        let
          left :: SubRecord ()
          left = mkSubRecord {}

          right :: SubRecord ()
          right = mkSubRecord {}

          result = hzipRight left right

          expected = {}
        result `shouldEqual` expected
      it "should work with empty right" do
        let
          left :: SubRecord ( foo :: String )
          left = mkSubRecord { foo: "hello world" }

          right :: SubRecord ()
          right = mkSubRecord {}

          result = hzipRight left right

          expected = {}
        result `shouldEqual` expected
      it "should work with empty left" do
        let
          left :: SubRecord ()
          left = mkSubRecord {}

          right :: SubRecord ( foo :: String )
          right = mkSubRecord { foo: "hello world" }

          result = hzipRight left right

          expected = { foo: Tuple (null :: Nullable String) (notNull "hello world") }
        result `shouldEqual` expected
      it "should work with same props" do
        let
          left :: SubRecord ( foo :: String )
          left = mkSubRecord { foo: "left" }

          right :: SubRecord ( foo :: String )
          right = mkSubRecord { foo: "right" }

          result = hzipRight left right

          expected = { foo: Tuple (notNull "left") (notNull "right") }
        result `shouldEqual` expected
      it "should work with diff props" do
        let
          left :: SubRecord ( foo :: Int )
          left = mkSubRecord { foo: 123 }

          right :: SubRecord ( foo :: String )
          right = mkSubRecord { foo: "right" }

          result = hzipRight left right

          expected = { foo: Tuple (notNull 123) (notNull "right") }
        result `shouldEqual` expected
    describe "hzipRightRandSR" do
      it "should work with empty" do
        let
          left = {}

          right :: SubRecord ()
          right = mkSubRecord {}

          result = hzipRight left right

          expected = {}
        result `shouldEqual` expected
      it "should work with empty right" do
        let
          left = { foo: "hello world" }

          right :: SubRecord ()
          right = mkSubRecord {}

          result = hzipRight left right

          expected = {}
        result `shouldEqual` expected
      it "should work with empty left" do
        let
          left = {}

          right :: SubRecord ( foo :: String )
          right = mkSubRecord { foo: "hello world" }

          result = hzipRight left right

          expected = { foo: Tuple (null :: Nullable String) (notNull "hello world") }
        result `shouldEqual` expected
      it "should work with same props" do
        let
          left = { foo: "left" }

          right :: SubRecord ( foo :: String )
          right = mkSubRecord { foo: "right" }

          result = hzipRight left right

          expected = { foo: Tuple "left" (notNull "right") }
        result `shouldEqual` expected
      it "should work with same two props" do
        let
          left = { foo: "left", bar: "leftbar" }

          right :: SubRecord ( foo :: String, bar :: String )
          right = mkSubRecord { foo: "right", bar: "rightbar" }

          result = hzipRight left right

          expected =
            { foo: Tuple "left" (notNull "right")
            , bar: Tuple "leftbar" (notNull "rightbar")
            }
        result `shouldEqual` expected
      it "should work with diff props" do
        let
          left = { foo: 123 }

          right :: SubRecord ( foo :: String )
          right = mkSubRecord { foo: "right" }

          result = hzipRight left right

          expected = { foo: Tuple 123 (notNull "right") }
        result `shouldEqual` expected
    describe "hzipRightSRandR" do
      it "should work with empty" do
        let
          left :: SubRecord ()
          left = mkSubRecord {}

          right = {}

          result = hzipRight left right

          expected = {}
        result `shouldEqual` expected
      it "should work with empty right" do
        let
          left :: SubRecord ( foo :: String )
          left = mkSubRecord { foo: "hello world" }

          right = {}

          result = hzipRight left right

          expected = {}
        result `shouldEqual` expected
      it "should work with empty left" do
        let
          left :: SubRecord ()
          left = mkSubRecord {}

          right = { foo: "hello world" }

          result = hzipRight left right

          expected = { foo: Tuple (null :: Nullable String) "hello world" }
        result `shouldEqual` expected
      it "should work with same props" do
        let
          left :: SubRecord ( foo :: String )
          left = mkSubRecord { foo: "left" }

          right = { foo: "right" }

          result = hzipRight left right

          expected = { foo: Tuple (notNull "left") "right" }
        result `shouldEqual` expected
      it "should work with diff props" do
        let
          left :: SubRecord ( foo :: Int )
          left = mkSubRecord { foo: 123 }

          right = { foo: "right" }

          result = hzipRight left right

          expected = { foo: Tuple (notNull 123) "right" }
        result `shouldEqual` expected
