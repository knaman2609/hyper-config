module Test.HyperWidgetConfig.Resolve.ConfigTree.Value (spec) where

import Test.Prelude
import Data.Maybe (Maybe(..))
import HyperWidgetConfig.Reify (reify)
import HyperWidgetConfig.Resolve.ConfigTree.Value (fromValue, getType, mkValue)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "mkValue + getType" do
    it "should get same type as we created with" do
      let
        input = 123

        value = mkValue input

        result = getType value

        expected = reify (pure input)
      result `shouldEqual` expected
  describe "mkValue + fromValue" do
    it "should get same type as we created with" do
      let
        input = "string"

        value = mkValue input

        result :: Maybe String
        result = fromValue value

        expected = Just "string"
      result `shouldEqual` expected
    it "should not get value of wrong type" do
      let
        input = "string"

        value = mkValue input

        result :: Maybe Int
        result = fromValue value

        expected = Nothing
      result `shouldEqual` expected
