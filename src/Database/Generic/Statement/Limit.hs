module Database.Generic.Statement.Limit where

import Database.Generic.Prelude
import Database.Generic.Statement.OrderBy (IsOrderedBy)

type Limit = Int

type Offset = Int

-- | Class of statements to which limit clauses can be added.
-- Statements must already have an order by clause.
class IsOrderedBy s => Limitable s where
  -- | Add a limit clause and maybe an offset.
  limitOffsetMay :: Limit -> Maybe Offset -> s -> s

-- | Add a limit clause without an offset.
limit :: Limitable s => Limit -> s -> s
limit l = limitOffsetMay l Nothing

-- | Add a limit clause with an offset.
limitOffset :: Limitable s => Limit -> Offset -> s -> s
limitOffset l = limitOffsetMay l . Just
