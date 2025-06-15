module Database.Generic.Database where

import Database.Generic.Prelude
import Database.Generic.Entity.SqlTypes (DbValue)

class Database db where
  type DbV db :: Type

data PostgreSQL

instance Database PostgreSQL where
  type DbV PostgreSQL = DbValue
