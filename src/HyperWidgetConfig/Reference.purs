module HyperWidgetConfig.Reference where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

data Reference a
  = Literal a
  | Reference String

derive instance eqReference :: Eq a => Eq (Reference a)

derive instance genericReference :: Generic (Reference a) _

instance showReference :: Show a => Show (Reference a) where
  show = genericShow
