module Database.Generic.Statement.Returning where

-- | Type of Haskell value is returned by executing a statement OR that would be
-- returned if the statement is modified via 'returning'.
type        Returning :: forall s a. s -> a
type family Returning s

-- | Statements for which the return type can be modified.
--
-- For example, you might modify a select statement to return a subset of fields.
type        ModifyReturning :: forall s1 s2 b. s1 -> b -> s2
type family ModifyReturning s1 b

class Returnable s1 s2 | s1 -> s2 where
  returning :: s1 -> s2
