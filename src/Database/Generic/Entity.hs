module Database.Generic.Entity where

import Database.Generic.Entity.FromSql (FromSqlValues)
import Database.Generic.Entity.FromSql qualified as FromSql
import Database.Generic.Entity.SqlValue (SqlValue)
import Database.Generic.Entity.ToSql (ToSqlFields, ToSqlValue, showType')
import Database.Generic.Entity.ToSql qualified as ToSql
import Database.Generic.Prelude

-- | An Entity can be converted to/from Sql and has a primary key.
class Entity a f | a -> f where
  primaryKey :: forall b. (ToSqlValue b, HasField f a b) => a -> b
  primaryKey = getField @f

  primaryKeyFieldName         ::               String
  default primaryKeyFieldName :: Typeable f => String
  primaryKeyFieldName = showType' @f

  entityFromSql         ::                    [SqlValue] -> a
  default entityFromSql :: FromSqlValues a => [SqlValue] -> a
  entityFromSql = FromSql.fromSqlValues

  entityToSql         ::                  a -> [(String, SqlValue)]
  default entityToSql :: ToSqlFields a => a -> [(String, SqlValue)]
  entityToSql = ToSql.toSqlFields
