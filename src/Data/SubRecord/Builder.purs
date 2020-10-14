-- | originated from https://raw.githubusercontent.com/rubenpieters/purescript-subrecord/13fae32237f79ac2236c2e1e00147c0d51fd8b72/src/Data/SubRecord/Builder.purs
module Data.SubRecord.Builder
  ( module SubRecord
  ) where

import Data.SubRecord
  ( Builder
  , build
  , insert
  , replace
  , modify
  , delete
  , rename
  , merge
  )
  as SubRecord
