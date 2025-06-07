module Database.Generic.Statement.Returning where

-- | Type of Haskell value returned from executing a statement.
type        IsReturning :: forall s a. s -> a
type family IsReturning s

-- | Statements for which the return type can be modified.
--
-- For example, you might modify a select statement to return a subset of fields.
type        ModifyReturnType :: forall s1 s2 b. s1 -> b -> s2
type family ModifyReturnType s1 b

-- | Update a statement so it is now returning Haskell values.
class Returning s1 s2 | s1 -> s2 where
  returning :: s1 -> s2

-- | Type of Haskell value in the table affected by the statement.
type        Row :: forall s a. s -> a
type family Row s
