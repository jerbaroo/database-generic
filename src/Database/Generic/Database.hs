module Database.Generic.Database where

import Database.Generic.Entity.SqlTypes (SqlTypeId(..))
import Database.Generic.Prelude

class Database db where
  columnType :: SqlTypeId -> String

data PostgreSQL

instance Database PostgreSQL where
  columnType SqlLongVarCharT = "VARCHAR"
  columnType SqlBigIntT      = "BIGINT"
  columnType _               = "undefined"

data SQLite

instance Database SQLite where
  columnType SqlLongVarCharT = "VARCHAR"
  columnType SqlBigIntT      = "BIGINT"
  columnType _               = "undefined"
