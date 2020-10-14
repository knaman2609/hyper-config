module Extra.Data.Function where

-- | `fn` must be a function with a type-class constraint.
-- | Partial application for type-class constraint - it passes undefined as
-- | type-class instance to `fn` and returns new function without type-class constraint.
-- | Useful for bypassing "dummy" instances like Union.
foreign import unsafeApplyWithUndefinedInstance :: forall fn fnWithoutConstraint. fn -> fnWithoutConstraint
