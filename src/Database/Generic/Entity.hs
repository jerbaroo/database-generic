module Database.Generic.Entity where

import Database.Generic.Entity.FromSql (FromSqlValues)
import Database.Generic.Entity.ToSql (HasSqlColumnNames, HasSqlColumnTypes, ToSqlValue, ToSqlValues)
import Database.Generic.Prelude
import Database.Generic.Table (TableName(TableName))
import Database.Generic.Field (FieldE, fieldE, fieldName)

-- | An 'Entity' can be converted to/from SQL and has a primary key.
--
-- For simple Haskell records with a single data constructor and named fields
-- you can derive an instance via 'Generic':
-- > data Person { name :: String, age :: Int }
-- >   deriving (Entity "name", Generic)
class ( FromSqlValues     a
      , HasSqlColumnNames a
      , HasSqlColumnTypes a
      , ToSqlValues       a
      ) => Entity f a | a -> f where

  -- TODO merge with primaryKeyField
  primaryKey :: forall b. (HasField f a b, ToSqlValue b) => a -> b
  primaryKey = getField @f

  primaryKeyField         ::               FieldE
  default primaryKeyField :: forall b. (HasField f a b, Typeable f) => FieldE
  primaryKeyField = fieldE @f @a @b

  tableName         ::               TableName
  default tableName :: Typeable a => TableName
  tableName = TableName $ toLower <$> showType @a

primaryKeyFieldName :: forall a f. Entity f a => String
primaryKeyFieldName = fieldName $ primaryKeyField @f @a
