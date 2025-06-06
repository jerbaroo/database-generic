module Database.Generic.Entity where

import Database.Generic.Entity.EntityName (EntityName(..))
import Database.Generic.Entity.Field (Field(..), field)
import Database.Generic.Entity.FieldName (FieldName, HasFieldName)
import Database.Generic.Entity.FromSql (FromSqlValues)
import Database.Generic.Entity.SqlFields (HasSqlFields)
import Database.Generic.Entity.ToSql (ToSqlValue, ToSqlValues)
import Database.Generic.Prelude

-- | An 'Entity' can be converted to/from SQL and has a primary key.
--
-- For simple Haskell records with a single data constructor and named fields
-- you can derive an instance via 'Generic' by specifing the primary key field:
-- > data Person { name :: String, age :: Int }
-- >   deriving (Entity "name", Generic)
class (FromSqlValues a, HasSqlFields a, ToSqlValues a)
  => Entity f a | a -> f where

  entityName         ::               EntityName
  default entityName :: Typeable a => EntityName
  entityName = EntityName $ toLower <$> showType @a

  primaryKeyField         ::
    forall b. (HasField f a b,                 ToSqlValue b) => Field f a b
  default primaryKeyField ::
    forall b. (HasField f a b, HasFieldName f, ToSqlValue b) => Field f a b
  primaryKeyField = field @f @a

-- | 'Entity' with additional type parameter 'b' (of 'HasField f a b') 'in scope.
type EntityP f a b = (Entity f a, HasField f a b, ToSqlValue b)

primaryKey :: forall a f b. (EntityP f a b) => a -> b
primaryKey = let (Field _ f) = primaryKeyField @_ @a in f

primaryKeyFieldName :: forall a f b. (EntityP f a b) => FieldName
primaryKeyFieldName = (primaryKeyField @_ @a).name
