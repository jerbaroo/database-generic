module Database.Generic.Entity where

import Database.Generic.Entity.EntityName (HasEntityName)
import Database.Generic.Entity.FromSql (FromSqlValues)
import Database.Generic.Entity.PrimaryKey (PrimaryKey, PrimaryKey')
import Database.Generic.Entity.SqlColumns (HasSqlColumns)
import Database.Generic.Entity.ToSql (ToSqlValue, ToSqlValues)

-- | An 'Entity' can be converted to/from SQL and has a primary key.
--
-- For simple Haskell records with a single data constructor and named fields
-- you only need to derive 'Generic' and 'PrimaryKey' to get an 'Entity' instance:
-- > data Person = Person { name :: String, age :: Int64 }
-- > deriving (Generic, PrimaryKey "name")
class (FromSqlValues a, HasEntityName a, HasSqlColumns a, PrimaryKey f a, ToSqlValues a) => Entity a f

instance (FromSqlValues a, HasEntityName a, HasSqlColumns a, PrimaryKey f a, ToSqlValues a) => Entity a f

-- | 'Entity' with type 'b' of primary key in scope.
type Entity' a f b = (Entity a f, PrimaryKey' a f b, ToSqlValue b)
