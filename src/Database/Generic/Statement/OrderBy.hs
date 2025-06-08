module Database.Generic.Statement.OrderBy where

import Database.Generic.Statement.Fields (FieldsOf)
import Database.Generic.Statement.Returning (IsReturning, Row)

-- | Statements with an order by clause.
class IsOrderedBy s

-- | Modify the statement type to reflect the statement has an order by clause.
type        ModifyOrderedBy :: forall s1 s2. s1 -> s2
type family ModifyOrderedBy s1

-- | Class of statements to which order by clauses can be added.
-- Statements must already be returning something, otherwise nothing to order.
class IsReturning s => OrderBy s where
  -- | Add an order by clause to a statement.
  orderBy :: forall fs a. (FieldsOf fs (Row s) a)
    => fs                -- ^ Fields to order by.
    -> s                 -- ^ The original statement.
    -> ModifyOrderedBy s -- ^ Statement now with an order by clause.
