module Extra.Data.Lens.Indexed where

import Prelude
import Data.Array as Array
import Data.Lens (Indexed(..), IndexedTraversal', Lens', itraverseOf)
import Data.Lens.Indexed (iwander, reindexed)
import Data.List (List(..))
import Data.List as List
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Newtype as Newtype
import Data.Profunctor (lcmap)
import Data.String (Pattern(..), joinWith, split)
import Data.Tuple (Tuple(..))

newtype Segments
  = Segments (List String)

newtype ReversedSegments
  = ReversedSegments (List String)

derive instance newtypeSegments :: Newtype Segments _

derive instance newtypeReversedSegments :: Newtype ReversedSegments _

instance semigroupSegments :: Semigroup Segments where
  append = Newtype.over2 Segments append

instance semigroupReversedSegments :: Semigroup ReversedSegments where
  append = Newtype.over2 ReversedSegments (flip append)

instance monoidSegments :: Monoid Segments where
  mempty = Segments Nil

instance monoidReversedSegments :: Monoid ReversedSegments where
  mempty = ReversedSegments Nil

class FlipSegments from to | from -> to, to -> from where
  flipSegments :: from -> to

instance flipSegmentsReverse :: FlipSegments Segments ReversedSegments where
  flipSegments = wrap <<< List.reverse <<< unwrap

instance flipSegmentsUnReverse :: FlipSegments ReversedSegments Segments where
  flipSegments = wrap <<< List.reverse <<< unwrap

class ToPath from where
  toPath :: from -> String

instance toPathSegments :: ToPath Segments where
  toPath = joinWith "." <<< Array.fromFoldable <<< unwrap

instance toPathReversedSegments :: ToPath ReversedSegments where
  toPath = toPath <<< flipSegments

class FromPath to where
  fromPath :: String -> to

instance fromPathSegments :: FromPath Segments where
  fromPath = wrap <<< List.fromFoldable <<< split (Pattern ".")

instance fromPathReversedSegments :: FromPath ReversedSegments where
  fromPath = flipSegments <<< fromPath

addSegment :: String -> ReversedSegments -> ReversedSegments
addSegment segment = Newtype.over ReversedSegments $ List.(:) segment

-- TODO: would be nice to switch Lens' to AffineTraversal, but there seems to not be a type for that
segmentify :: forall i a b. Monoid i => Lens' a b -> IndexedTraversal' i a b
segmentify l (Indexed piab) = l pab
  where
  pab = piab # lcmap \a -> Tuple mempty a

isegmentify :: forall a b. IndexedTraversal' String a b -> IndexedTraversal' ReversedSegments a b
isegmentify t = reindexed fromPath t

icombine :: forall i j k a b c. (i -> j -> k) -> IndexedTraversal' i a b -> IndexedTraversal' j b c -> IndexedTraversal' k a c
icombine fn t1 t2 = iwander f3
  where
  f3 :: forall f. Applicative f => (k -> c -> f c) -> a -> f a
  f3 if3 = f1 if1
    where
    f1 = itraverseOf t1

    f2 = itraverseOf t2

    if1 i = f2 if2
      where
      if2 j = if3 k
        where
        k = fn i j

icombineS :: forall i a b c. Semigroup i => IndexedTraversal' i a b -> IndexedTraversal' i b c -> IndexedTraversal' i a c
icombineS t1 t2 = icombine append t1 t2
