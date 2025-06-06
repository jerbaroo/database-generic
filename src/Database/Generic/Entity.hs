module Database.Generic.Entity where

import Database.Generic.Entity.EntityName (EntityName(..))
import Database.Generic.Entity.PrimaryKey (HasPrimaryKey, HasPrimaryKeyP)
import Database.Generic.Entity.FromSql (FromSqlValues)
import Database.Generic.Entity.SqlColumns (HasSqlColumns)
import Database.Generic.Entity.ToSql (ToSqlValue, ToSqlValues)
import Database.Generic.Prelude

-- | An 'Entity' can be converted to/from SQL and has a primary key.
--
-- For simple Haskell records with a single data constructor and named fields
-- you can derive an instance via 'Generic' and specifing the primary key field:
-- > data Person = Person { name :: String, age :: Int64 }
-- > deriving (Generic)
-- > deriving HasPKField via PK "name" Person
class (FromSqlValues a, HasPrimaryKey a, HasSqlColumns a, ToSqlValues a) => Entity a where

  -- | Identifier for the collection in the database (i.e. SQL table name)
  entityName         ::               EntityName
  default entityName :: Typeable a => EntityName
  entityName = EntityName $ toLower <$> showType @a

-- | 'Entity' with additional types of primary key in scope.
type EntityP a f b = (Entity a, HasPrimaryKeyP a f b, ToSqlValue b)
