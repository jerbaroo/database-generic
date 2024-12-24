module Database.Generic.Serialize where

import Database.Generic.Database (PostgreSQL, SQLite)
import Database.Generic.Entity.SqlTypes (SqlTypeId(..), SqlValue(..))
import Database.Generic.Prelude

class Serialize a db where
  serialize :: a -> String

instance Serialize SqlValue db where
  serialize (SqlString s) = "'" <> s <> "'"
  serialize (SqlInt64 i)  = show i
  serialize s = "Serialize SqlValue db: Not implemented for " <> show s

instance Serialize SqlTypeId PostgreSQL where
  serialize SqlLongVarCharT = "VARCHAR"
  serialize SqlBigIntT      = "BIGINT"
  serialize _               = "undefined"

instance Serialize SqlTypeId SQLite where
  serialize SqlLongVarCharT = "VARCHAR"
  serialize SqlBigIntT      = "BIGINT"
  serialize _               = "undefined"
