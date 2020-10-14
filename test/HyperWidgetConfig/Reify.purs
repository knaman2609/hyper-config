module Test.HyperWidgetConfig.Reify (spec) where

import Test.Prelude
import Data.SubRecord (SubRecord)
import Foreign.Object (fromHomogeneous)
import HyperWidgetConfig.Reify (RuntimeType(..), reify)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

type Row
  = ( someInt :: Int
    , someString :: String
    )

type R
  = Record Row

type S
  = SubRecord Row

type R2
  = { inner :: R
    | Row
    }

type S2
  = SubRecord
      ( inner :: S
      | Row
      )

spec :: Spec Unit
spec = do
  describe "reifyRecord" do
    it "should pass for 1-level deep" do
      let
        result = reify (Proxy :: Proxy R)

        expected =
          RuntimeRecord
            $ fromHomogeneous
                { someInt: RuntimeInt
                , someString: RuntimeString
                }
      result `shouldEqual` expected
    it "should pass for 2-level deep" do
      let
        result = reify (Proxy :: Proxy R2)

        expected =
          RuntimeRecord
            $ fromHomogeneous
                { someInt: RuntimeInt
                , someString: RuntimeString
                , inner:
                    RuntimeRecord
                      $ fromHomogeneous
                          { someInt: RuntimeInt
                          , someString: RuntimeString
                          }
                }
      result `shouldEqual` expected
  describe "reifySubRecord" do
    it "should pass for 1-level deep" do
      let
        result = reify (Proxy :: Proxy S)

        expected =
          RuntimeRecord
            $ fromHomogeneous
                { someInt: RuntimeInt
                , someString: RuntimeString
                }
      result `shouldEqual` expected
    it "should pass for 2-level deep" do
      let
        result = reify (Proxy :: Proxy S2)

        expected =
          RuntimeRecord
            $ fromHomogeneous
                { someInt: RuntimeInt
                , someString: RuntimeString
                , inner:
                    RuntimeRecord
                      $ fromHomogeneous
                          { someInt: RuntimeInt
                          , someString: RuntimeString
                          }
                }
      result `shouldEqual` expected
