module Database.Generic.Statement.OrderBy where

import Database.Generic.Statement.Fields (FieldsOf)
import Database.Generic.Statement.Returning (IsReturning, Row)

class IsReturning s => OrderBy s where
  -- | Update a statement 's' to order returned data by fields 'fs'.
  orderBy :: forall fs a. (FieldsOf fs (Row s) a)
    => fs -- ^ Fields to order by.
    -> s  -- ^ The original statement.
    -> s  -- ^ The type of the statement is not altered.
