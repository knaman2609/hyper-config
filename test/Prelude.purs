module Test.Prelude
  ( module Test.Spec
  , module Test.Spec.QuickCheck
  , module Extra.Debug
  , module Prelude
  , itCompileTimeCheck
  , itNotImplemented
  , shouldEqualExcept
  ) where

import Prelude as Prelude
import Control.Monad.Except (Except)
import Data.Function (on)
import Effect.Aff (Aff)
import Extra.Debug (class CompileTimeCheck, class NotImplemented, runCompileTimeCheck, any) as Extra.Debug
import Test.Spec (Spec, describe, it, pending, pending') as Test.Spec
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck) as Test.Spec.QuickCheck
import Test.Utils (ShowExcept(..))

itCompileTimeCheck :: String -> (Extra.Debug.CompileTimeCheck => Aff Prelude.Unit) -> Test.Spec.Spec Prelude.Unit
itCompileTimeCheck title ctc = Prelude.pure (Extra.Debug.runCompileTimeCheck ctc)

itNotImplemented :: String -> (Extra.Debug.NotImplemented => Aff Prelude.Unit) -> Test.Spec.Spec Prelude.Unit
itNotImplemented title _ = Test.Spec.pending (Prelude.append "not implemented: " title)

shouldEqualExcept :: forall e a. Prelude.Show e => Prelude.Show a => Prelude.Eq a => Prelude.Eq e => Except e a → Except e a → Aff Prelude.Unit
shouldEqualExcept = shouldEqual `on` ShowExcept
