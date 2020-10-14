module HyperWidgetConfig.Error where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Symbol (SProxy(..))
import HyperWidgetConfig.Reify (RuntimeType)
import Prim.Row (class Lacks, class Nub, class Union)
import Record as Record
import Record.Unsafe.Union (unsafeUnion)

type ConfigError
  = { path :: String
    , reference :: String
    , error :: ResolveError
    }

data ResolveError
  = NotFound
  | Conflict { conflictAt :: String }
  | WrongType
    { expected :: RuntimeType
    , actual :: RuntimeType
    }
  | NotComponentDefinition -- TODO: expand WrongType or Conflict to replace this?
  | WrongSection -- TODO: get rid of NotComponentDefinition in favor of wrong section?
  | CircularReference
  | MissingReference
  | WrongComponentReferenceType
    { expected :: String
    , actual :: String
    }

instance semigroupResolveError :: Semigroup ResolveError where
  -- if there is any other error than NotFound it means it WAS actually found,
  -- just got an error on some alternative. So it makes NotFound of the least priority.
  append NotFound e = e
  append e NotFound = e
  append e _ = e

derive instance eqResolveError :: Eq ResolveError

derive instance genericResolveError :: Generic ResolveError _

instance showResolveError :: Show ResolveError where
  show = genericShow

type ReferenceErrorEnvelope' info e
  = ErrorEnvelope ( reference :: String | info ) e

type ReferenceErrorEnvelope
  = ReferenceErrorEnvelope' () ResolveError

packReferenceError :: forall e. e -> String -> ReferenceErrorEnvelope' () e
packReferenceError error reference = { error, reference }

type ErrorEnvelope info e
  = { error :: e
    | info
    }

type PropertyErrorEnvelope info e
  = ErrorEnvelope ( path :: String | info ) e

-- TODO: it makes more sense to swap argument order e -> Record info -> ErrorEnvelope info e
packError ::
  forall e info.
  Lacks "error" info =>
  e -> Record info -> ErrorEnvelope info e
packError error = Record.insert (SProxy :: SProxy "error") error

packError_ :: forall e. e -> ErrorEnvelope () e
packError_ error = { error }

attachInfo ::
  forall info info' info'' info''' e.
  Union info info' info'' =>
  Nub info'' info''' =>
  Lacks "error" info =>
  Lacks "error" info' =>
  Record info' -> ErrorEnvelope info e -> ErrorEnvelope info''' e
attachInfo info envelope = unsafeUnion info envelope

packPropertyError :: forall e. e -> String -> PropertyErrorEnvelope () e
packPropertyError error path = { error, path }

attachPropertyInfo :: forall e info. Lacks "path" info => String -> ErrorEnvelope info e -> PropertyErrorEnvelope info e
attachPropertyInfo path = Record.insert (SProxy :: SProxy "path") path
