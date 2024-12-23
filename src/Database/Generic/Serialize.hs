module Database.Generic.Serialize where

import Database.Generic.Database (PostgreSQL, SQLite)
import Database.Generic.Entity (Entity)
import Database.Generic.Entity qualified as Entity
import Database.Generic.Entity.SqlTypes (SqlTypeId(..), SqlValue(..))
import Database.Generic.Prelude

class Serialize a db where
  serialize :: a -> String

instance Serialize SqlValue db where
  serialize (SqlString s) = "'" <> s <> "'"
  serialize _ = "Serialize SqlValue db: Not implemented"

newtype TableName = TableName String

tableName :: forall a f. Entity f a => TableName
tableName = TableName $ toLower <$> Entity.tableName @_ @a

instance Serialize TableName db where
  serialize (TableName s) = s

instance Serialize SqlTypeId PostgreSQL where
  serialize SqlLongVarCharT = "VARCHAR"
  serialize SqlBigIntT      = "BIGINT"
  serialize _               = "undefined"

instance Serialize SqlTypeId SQLite where
  serialize SqlLongVarCharT = "VARCHAR"
  serialize SqlBigIntT      = "BIGINT"
  serialize _               = "undefined"
