module Database.Generic.Statement.Returning where

import Database.Generic.Statement.Fields (FieldsOf)

-- | Type of value returned from executing a statement.
type        IsReturning :: forall s a. s -> a
type family IsReturning s

-- | Modify the type of the statement to reflect it returns values of type 'r'.
--
-- NOTE: don't use this directly, you likely want 'returning' or 'returningFields'.
type        ModifyReturnType :: forall s1 s2 r. s1 -> r -> s2
type family ModifyReturnType s1 r

class Returning s1 s2 | s1 -> s2 where
  -- | Update a statement so it will return values on execution.
  -- This is akin to applying an SQL RETURNING clause.
  returning :: s1 -> s2

class ReturningFields s where
  -- | Update a statement so it will return a subset of fields on execution.
  returningFields :: forall f a. (FieldsOf f (Row s) a)
    => s                    -- ^ The original statement.
    -> f                    -- ^ Fields to select, which can be parsed into a 'a'.
    -> ModifyReturnType s a -- ^ A statement now returning values of type 'a'.

infixl 4 ==>

-- | Infix version of 'returningFields'.
(==>) :: forall s f a.
  (FieldsOf f (Row s) a, ReturningFields s)
  => s -> f -> ModifyReturnType s a
(==>) = returningFields

-- | Type of 'Entity' in the table affected by the statement.
--
-- This allows the type-checker to determine all the possible returnable fields.
type        Row :: forall s a. s -> a
type family Row s
