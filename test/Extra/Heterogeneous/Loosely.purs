module Test.Extra.Heterogeneous.Loosely (spec) where

import Test.Prelude
import Extra.Heterogeneous.Loosely (LooselyMapping(..))
import Heterogeneous.Mapping (hmap)
import Test.Spec.Assertions (shouldEqual)

intToString :: Int -> String
intToString = show

spec :: Spec Unit
spec =
  describe "LooselyMapping" do
    it "should work with functions" do
      let
        result = hmap (LooselyMapping intToString) { a: 123, b: 123.0, c: "123" }
      result `shouldEqual` { a: "123", b: 123.0, c: "123" }
