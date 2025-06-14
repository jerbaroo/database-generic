module Database.Generic.Entity where

import Database.Generic.Entity.EntityName (HasEntityName)
import Database.Generic.Entity.FromSql (FromDbValues)
import Database.Generic.Entity.PrimaryKey (PrimaryKey, PrimaryKey')
import Database.Generic.Entity.SqlColumns (HasDbColumns)
import Database.Generic.Entity.ToSql (ToDbValue, ToDbValues)

-- | An 'Entity' can be converted to/from database-reprentation.
--
-- For simple Haskell records with a single data constructor and named fields
-- you only need to derive 'Generic' and 'PrimaryKey' to get an 'Entity' instance:
-- > data Person = Person { age :: Int64, name :: String }
-- > deriving (Generic, PrimaryKey "name")
class (FromDbValues a, HasDbColumns a, HasEntityName a, PrimaryKey f a, ToDbValues a) => Entity a f

instance (FromDbValues a, HasDbColumns a, HasEntityName a, PrimaryKey f a, ToDbValues a) => Entity a f

-- | 'Entity' with type 'b' of primary key in scope.
type Entity' a f b = (Entity a f, PrimaryKey' a f b, ToDbValue b)
