module Database.Generic.Database where

import Database.Generic.Prelude
import Database.Generic.Entity.DbTypes (DbValueN)

class Database db where
  -- | Type representing one cell in a row, as returned from statement execution.
  --
  -- Fields of your data types need to be parsable from this type via 'FromDbValues'.
  type DbV db :: Type

data PostgreSQL

instance Database PostgreSQL where
  type DbV PostgreSQL = DbValueN
