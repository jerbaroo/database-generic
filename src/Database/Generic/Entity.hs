module Database.Generic.Entity where

import Database.Generic.Entity.FromSql (FromSqlValues)
import Database.Generic.Entity.ToSql (HasSqlColumns, ToSqlValue, ToSqlValues)
import Database.Generic.Prelude
import Database.Generic.Table (TableName(TableName))
import Database.Generic.Field (Field(..), HasFieldName, field, fieldName)

-- | An 'Entity' can be converted to/from SQL and has a primary key.
--
-- For simple Haskell records with a single data constructor and named fields
-- you can derive an instance via 'Generic' needing only to specify primary key:
-- > data Person { name :: String, age :: Int }
-- >   deriving (Entity "name", Generic)
class (FromSqlValues a, HasSqlColumns a, ToSqlValues a
      ) => Entity f a | a -> f where

  primaryKeyField         :: forall b. (HasField f a b,                 ToSqlValue b) => Field f a b
  default primaryKeyField :: forall b. (HasField f a b, HasFieldName f, ToSqlValue b) => Field f a b
  primaryKeyField = field @f @a

  tableName         ::               TableName
  default tableName :: Typeable a => TableName
  tableName = TableName $ toLower <$> showType @a

-- | 'Entity' but with additional type parameter 'b' in scope.
type EntityP f a b = (Entity f a, HasField f a b, ToSqlValue b)

primaryKey :: forall a f b. (EntityP f a b) => a -> b
primaryKey = let (Field _ f) = primaryKeyField @_ @a in f

primaryKeyFieldName :: forall a f b. (EntityP f a b) => String
primaryKeyFieldName = (primaryKeyField @_ @a).name
