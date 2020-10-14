module Extra.Data.Validation.Semigroup where

import Prelude
import Data.Validation.Semigroup (V, andThen)
import Prelude as Prelude

class V m where
  pure :: forall e a. Semigroup e => a -> m e a
  bind :: forall e a b. m e a -> (a -> m e b) -> m e b

instance vV :: V V where
  pure = Prelude.pure
  bind = andThen
