module Test.Extra.Data.Row (spec) where

import Test.Prelude
import Extra.Data.Row (class HasProp, class HasPropAny, class OptionalPropIf)
import Prim.Boolean (False, True)

spec :: Spec Unit
spec = do
  describe "HasPropAny" do
    itCompileTimeCheck "should True" do
      pure $ runCompileTimeCheck (any :: HasPropAny "test" ( test :: String ) True => Unit)
  describe "HasProp" do
    itCompileTimeCheck "should True" do
      pure $ runCompileTimeCheck (any :: HasProp "test" String ( test :: String ) True => Unit)
    itCompileTimeCheck "should False" do
      pure $ runCompileTimeCheck (any :: HasProp "test" String ( asdf :: String ) False => Unit)
    itCompileTimeCheck "should True when others present" do
      pure $ runCompileTimeCheck (any :: HasProp "test" String ( abc :: Int, test :: String ) True => Unit)
    itCompileTimeCheck "should True when others present regardless of order" do
      pure $ runCompileTimeCheck (any :: HasProp "test" String ( test :: String, zxc :: String ) True => Unit)
    itCompileTimeCheck "should True for other types" do
      pure $ runCompileTimeCheck (any :: HasProp "test2" Int ( test2 :: Int ) True => Unit)
  describe "OptionalPropIf" do
    itCompileTimeCheck "should not complain if present" do
      pure $ runCompileTimeCheck (any :: OptionalPropIf True "test" ( test :: String, foo :: Int ) ( test :: String, foo :: Int ) => Unit)
    itCompileTimeCheck "should not complain if absent" do
      pure $ runCompileTimeCheck (any :: OptionalPropIf True "test" ( test :: String, foo :: Int ) ( foo :: Int ) => Unit)
    itCompileTimeCheck "should not complain if present and not optional" do
      pure $ runCompileTimeCheck (any :: OptionalPropIf False "test" ( test :: String, foo :: Int ) ( test :: String, foo :: Int ) => Unit)
