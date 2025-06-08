module Database.Generic.Statement.Returning where

import Database.Generic.Statement.Fields (FieldsOf)

-- | Statements that return something on execution.
class IsReturning s

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
  -- | Update a statement 's' to return fields 'fs' (parsed into 'a's).
  returningFields :: forall fs a. (FieldsOf fs (Row s) a)
    => s                    -- ^ The original statement.
    -> fs                   -- ^ Fields to select, parsed into 'a's.
    -> ModifyReturnType s a -- ^ Statement now returning 'a's.

infixl 4 ==>

-- | Infix version of 'returningFields'.
(==>) :: forall s f b.
  (FieldsOf f (Row s) b, ReturningFields s)
  => s -> f -> ModifyReturnType s b
(==>) = returningFields

-- | Type of 'Entity' in the table affected by the statement.
--
-- This allows the type-checker to determine all the possible returnable fields.
type        Row :: forall s a. s -> a
type family Row s
