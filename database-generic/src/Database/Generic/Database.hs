module Database.Generic.Database where

import Database.Generic.Prelude
import Database.Generic.Entity.DbTypes (DbValue)

class Database db where
  -- | Type representing one cell in a row, as returned upon executing statements.
  --
  -- Needs to be parsable via 'FromDbValues', into the fields of your data types.
  type DbV db :: Type

data PostgreSQL

instance Database PostgreSQL where
  type DbV PostgreSQL = DbValue
