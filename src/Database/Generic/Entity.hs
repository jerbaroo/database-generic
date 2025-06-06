{-# LANGUAGE UndecidableInstances #-}

module Database.Generic.Entity where

import Database.Generic.Entity.EntityName (HasEntityName)
import Database.Generic.Entity.PrimaryKey (HasPrimaryKey, HasPrimaryKeyP)
import Database.Generic.Entity.FromSql (FromSqlValues)
import Database.Generic.Entity.SqlColumns (HasSqlColumns)
import Database.Generic.Entity.ToSql (ToSqlValue, ToSqlValues)

-- | An 'Entity' can be converted to/from SQL and has a primary key.
--
-- For simple Haskell records with a single data constructor and named fields
-- you only need to derive 'Generic' and 'PrimaryKey' to get an 'Entity' instance:
-- > data Person = Person { name :: String, age :: Int64 }
-- > deriving Generic
-- > deriving PrimaryKey via PK "name" Person
class (FromSqlValues a, HasPrimaryKey a, HasSqlColumns a, HasEntityName a, ToSqlValues a) => Entity a where

instance (FromSqlValues a, HasPrimaryKey a, HasSqlColumns a, HasEntityName a, ToSqlValues a) => Entity a

-- | 'Entity' with additional types of primary key in scope.
type EntityP a f b = (Entity a, HasPrimaryKeyP a f b, ToSqlValue b)
