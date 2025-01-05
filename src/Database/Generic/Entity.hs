module Database.Generic.Entity where

import Database.Generic.Entity.FromSql (FromSqlValues)
import Database.Generic.Entity.ToSql (HasSqlFieldNames, HasSqlFieldTypes, ToSqlValue, ToSqlValues)
import Database.Generic.Entity.ToSql qualified as ToSql
import Database.Generic.Prelude
import Database.Generic.Table (TableName(TableName))
import Database.Generic.Field (FieldE, fieldE, fieldType)

-- | An Entity can be converted to/from SQL, and has primary key and table name.
class ( FromSqlValues    a
      , HasSqlFieldNames a -- TODO merge with HasSqlFieldTypes into HasSqlFields
      , HasSqlFieldTypes a
      , ToSqlValues      a
      ) => Entity f a | a -> f where

  -- TODO merge with primaryKeyField
  primaryKey :: forall b. (HasField f a b, ToSqlValue b) => a -> b
  primaryKey = getField @f

  primaryKeyField         ::               FieldE
  default primaryKeyField :: forall b. (HasField f a b, Typeable f) => FieldE
  primaryKeyField = fieldE @f @a @b

  tableName         ::               TableName
  default tableName :: Typeable a => TableName
  tableName = TableName $ toLower <$> ToSql.showType @a

primaryKeyFieldName :: forall a f. Entity f a => String
primaryKeyFieldName = fieldType $ primaryKeyField @f @a
