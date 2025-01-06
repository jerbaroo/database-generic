module Database.Generic.Entity where

import Database.Generic.Entity.FromSql (FromSqlValues)
import Database.Generic.Entity.ToSql (HasSqlColumnNames, HasSqlColumnTypes, ToSqlValues)
import Database.Generic.Prelude
import Database.Generic.Table (TableName(TableName))
import Database.Generic.Field (Field, field, fieldName)

-- | An 'Entity' can be converted to/from SQL and has a primary key.
--
-- For simple Haskell records with a single data constructor and named fields
-- you can derive an instance via 'Generic' needing only to specify primary key:
-- > data Person { name :: String, age :: Int }
-- >   deriving (Entity "name", Generic)
class ( FromSqlValues     a
      , HasSqlColumnNames a
      , HasSqlColumnTypes a
      , ToSqlValues       a
      ) => Entity f a | a -> f where

  primaryKey         :: forall b. Field f a b
  default primaryKey :: forall b. (HasField f a b, Typeable f) => Field f a b
  primaryKey = field

  tableName         ::               TableName
  default tableName :: Typeable a => TableName
  tableName = TableName $ toLower <$> showType @a

primaryKeyFieldName :: forall a f. Entity f a => String
primaryKeyFieldName = fieldName $ primaryKey @f @a
