module Extra.Data.Lens.Prism
  ( eitherPrisms
  , eitherPrisms3
  ) where

import Control.Alt ((<$>), (<|>))
import Data.Either (Either, choose, either)
import Data.Either.Nested (Either3, either3, in1, in2, in3)
import Data.Lens (Prism', prism', review, (^?))

eitherPrisms :: forall s a b. Prism' s a -> Prism' s b -> Prism' s (Either a b)
eitherPrisms _a _b =
  prism'
    ( either
        (review _a)
        (review _b)
    ) \v ->
    choose
      (v ^? _a)
      (v ^? _b)

eitherPrisms3 :: forall s a b c. Prism' s a -> Prism' s b -> Prism' s c -> Prism' s (Either3 a b c)
eitherPrisms3 _a _b _c =
  prism'
    ( either3
        (review _a)
        (review _b)
        (review _c)
    ) \v ->
    (in1 <$> (v ^? _a))
      <|> (in2 <$> (v ^? _b))
      <|> (in3 <$> (v ^? _c))
