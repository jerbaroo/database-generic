module Database.Generic.Entity where

import Database.Generic.Entity.FromSql (FromSqlValues)
import Database.Generic.Entity.FromSql qualified as FromSql
import Database.Generic.Entity.SqlTypes (SqlTypeId, SqlValue)
import Database.Generic.Entity.ToSql (HasSqlFieldNames, HasSqlFieldTypes, ToSqlValue, ToSqlValues)
import Database.Generic.Entity.ToSql qualified as ToSql
import Database.Generic.Prelude

-- | An Entity can be converted to/from Sql and has a primary key.
class Entity f a | a -> f where
  primaryKey :: forall b. (HasField f a b, ToSqlValue b) => a -> b
  primaryKey = getField @f

  primaryKeyFieldName         ::               String
  default primaryKeyFieldName :: Typeable f => String
  primaryKeyFieldName = ToSql.showType' @f

  tableName         ::               String
  default tableName :: Typeable a => String
  tableName = ToSql.showType @a

  fromSqlValues         ::                    [SqlValue] -> a
  default fromSqlValues :: FromSqlValues a => [SqlValue] -> a
  fromSqlValues = FromSql.fromSqlValues

  toSqlValues         ::                  a -> [SqlValue]
  default toSqlValues :: ToSqlValues a => a -> [SqlValue]
  toSqlValues = ToSql.toSqlValues

  sqlFieldNames         ::                       [String]
  default sqlFieldNames :: HasSqlFieldNames a => [String]
  sqlFieldNames = ToSql.sqlFieldNames @a

  sqlFieldTypes         ::                       [SqlTypeId]
  default sqlFieldTypes :: HasSqlFieldTypes a => [SqlTypeId]
  sqlFieldTypes = ToSql.sqlFieldTypes @a
