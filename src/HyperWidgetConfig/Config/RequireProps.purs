module HyperWidgetConfig.Config.RequireProps where

import Data.SubRecord (SubRecord)
import Data.SubRecord.Heterogeneous.Nullable (class HToNullable)
import Extra.Data.Row (class HasProp)
import Extra.Debug (class CompileTimeCheck, any)
import Extra.Symbol (class AppendPath)
import Prim.Boolean (False, True, kind Boolean)
import Prim.Row as Row
import Prim.RowList (class RowToList, Nil, kind RowList)
import Prim.RowList as RL
import Type.Row (class ListToRow)

inferRequiredProps' :: forall props required. CompileTimeCheck => RequireProps_ props required => props -> required
inferRequiredProps' _ = any

isRequiredProps' :: forall props required. CompileTimeCheck => RequireProps_ props required => props -> required -> Boolean
isRequiredProps' _ _ = true

-- | This type-class converts optional props into required props.
class RequireProps_ props required | props -> required

instance requiredProps_ :: RequireProps__ "" props required => RequireProps_ (SubRecord props) (Record required)

class RequireProps__ (path :: Symbol) (props :: # Type) (required :: # Type) | props -> required

instance requiredProps__ ::
  ( RowToList from lfrom
  , HasProp "component" String from hasComponentProp
  -- , HasPropAny "states" from hasStatesProp
  -- , OptionalPropIf hasStatesProp "states" to' to -- why doesn't it work??
  , RequiredPropsRowList path hasComponentProp lfrom to
  ) =>
  RequireProps__ path from to

-- `path` is only to get a more helpful error message when something goes wrong
class RequiredPropsRowList (path :: Symbol) (hasComponentProp :: Boolean) (lfrom :: RowList) (to :: # Type) | hasComponentProp lfrom -> to

instance requiredPropsRowListNil ::
  RequiredPropsRowList prop False Nil ()
-- TODO: states prop should be optional
else instance requiredPropsRowListNoComponentStates ::
  RequiredPropsRowList path False tfrom tto =>
  RequiredPropsRowList
    path
    False
    (RL.Cons "states" (SubRecord dfrom) tfrom)
    ( states :: SubRecord dfrom | tto )
-- first-level of components must be present
else instance requiredPropsRowListNoComponentSR ::
  ( AppendPath path prop path'
  , Row.Cons prop (Record dto) tto to
  , RequireProps__ path' dfrom dto
  , RequiredPropsRowList path False tfrom tto
  ) =>
  RequiredPropsRowList
    path
    False
    (RL.Cons prop (SubRecord dfrom) tfrom)
    to
else instance requiredPropsRowListNoComponent ::
  ( Row.Cons prop v tto to
  , RequiredPropsRowList path False tfrom tto
  ) =>
  RequiredPropsRowList
    path
    False
    (RL.Cons prop v tfrom)
    to

instance requiredPropsRowListComponent ::
  ( ListToRow lfrom from
  , Row.Cons "component" String tfrom from
  , HToNullable (SubRecord tfrom) (Record tto)
  , Row.Cons "component" String tto to
  ) =>
  RequiredPropsRowList
    path
    True
    lfrom
    to
