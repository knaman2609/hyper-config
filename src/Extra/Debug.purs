module Extra.Debug where

import Prelude
import Effect (Effect)
import Effect.Aff (Aff, throwError)
import Effect.Exception (throw)
import Effect.Unsafe (unsafePerformEffect)
import Extra.Data.Function (unsafeApplyWithUndefinedInstance)
import Prim.TypeError (class Warn, Text)
import Unsafe.Coerce (unsafeCoerce)

class CompileTimeCheck

any :: forall a. CompileTimeCheck => a
any = unsafeCoerce "for compile-time checks only"

-- | Example:
-- | ```
-- | _ = runCompileTimeCheck (any :: HasProp "test" String ( test :: String ) True => Unit)
-- | ```
-- | to check that HasProp is resolved with these arguments.
runCompileTimeCheck :: forall a. (CompileTimeCheck => a) -> Unit
runCompileTimeCheck _ = unit

class NotImplemented

runNotImplemented :: forall a. (NotImplemented => a) -> Unit
runNotImplemented _ = unit

class NotImplementedStrict a where
  -- | for propagating the notion of "notimplementancy" up until we explicitly handle it somehow
  -- | or make compiler fail if we don't.
  notImplementedStrict :: NotImplemented => a

instance notImplementedStrictInst :: NotImplementedStrict a where
  notImplementedStrict = f notImplemented
    where
    f :: (Warn (Text "NotImplemented") => a) -> a
    f = unsafeApplyWithUndefinedInstance

class NotImplementedValue a where
  -- | for debugging
  notImplemented :: Warn (Text "NotImplemented") => a

-- notice that we must rely only on type equality here, we cannot use any type-class constraints
instance notImplementedEffect :: NotImplementedValue (Effect a) where
  notImplemented = throw notImplemented
else instance notImplementedAff :: NotImplementedValue (Aff a) where
  notImplemented = throwError notImplemented
else instance notImplementedFunction :: NotImplementedValue (a -> b) where
  notImplemented _ = unsafePerformEffect notImplemented
else instance notImplementedAny :: NotImplementedValue a where
  notImplemented = unsafeCoerce "notImplemented"
