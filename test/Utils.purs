module Test.Utils where

import Prelude
import Control.Monad.Except (Except, runExcept)
import Data.Function (on)
import Data.Newtype (class Newtype, unwrap)

newtype ShowExcept e a
  = ShowExcept (Except e a)

derive instance newtypeShowExcept :: Newtype (ShowExcept e a) _

instance eqShowExcept :: (Eq e, Eq a) =>Eq (ShowExcept e a) where
  eq = eq `on` (runExcept <<< unwrap)

instance showShowExcept :: (Show e, Show a) => Show (ShowExcept e a) where
  show = show <<< runExcept <<< unwrap
