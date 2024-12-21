module Database.Generic.Database where

import Database.Generic.Entity.SqlTypes (SqlType(..))
import Database.Generic.Prelude

class Database db where
  columnType :: SqlType -> String

data SQLite

instance Database SQLite where
  columnType SqlString = "TEXT"
  columnType SqlInt64  = "BIGINT"
