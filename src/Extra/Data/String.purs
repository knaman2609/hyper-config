module Extra.Data.String (split1) where

import Prelude
import Data.Array.NonEmpty (NonEmptyArray, cons', fromArray)
import Data.Maybe (fromJust)
import Data.String (Pattern(..), split)
import Partial.Unsafe (unsafePartial)

split1 :: Pattern -> String -> NonEmptyArray String
split1 (Pattern "") "" = cons' "" []

-- `split` always returns non empty array except for the case with empty pattern and empty content
split1 p s = unsafePartial fromJust $ fromArray $ split p s
