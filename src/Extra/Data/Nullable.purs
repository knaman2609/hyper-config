module Extra.Data.Nullable (unsafeFromNullable) where

import Data.Nullable (Nullable)
import Unsafe.Coerce (unsafeCoerce)

unsafeFromNullable :: forall a. Nullable a -> a
unsafeFromNullable = unsafeCoerce
