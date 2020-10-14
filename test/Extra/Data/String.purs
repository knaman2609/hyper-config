module Test.Extra.Data.String (spec) where

import Test.Prelude
import Data.Array.NonEmpty (cons')
import Data.String (Pattern(..))
import Extra.Data.String (split1)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec =
  describe "split1" do
    it "should not crash for empty input" do
      let
        result = split1 (Pattern ".") ""
      result `shouldEqual` cons' "" []
    it "should not crash for non-empty but no matches" do
      let
        result = split1 (Pattern ".") "qwer"
      result `shouldEqual` cons' "qwer" []
    it "should not crash for non-empty with matches" do
      let
        result = split1 (Pattern ".") "qw.er"
      result `shouldEqual` cons' "qw" [ "er" ]
    it "should not crash for empty input with empty pattern" do
      let
        result = split1 (Pattern "") ""
      result `shouldEqual` cons' "" []
    it "should not crash for non-empty input with empty pattern" do
      let
        result = split1 (Pattern "") "qwer"
      result `shouldEqual` cons' "q" [ "w", "e", "r" ]
