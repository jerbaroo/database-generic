{-# LANGUAGE UndecidableInstances #-}

module Database.Generic.Entity where

import Database.Generic.Entity.EntityName (EntityName(..))
import Database.Generic.Entity.Field (Field(..), field)
import Database.Generic.Entity.FieldName (FieldName)
import Database.Generic.Entity.FromSql (FromSqlValues)
import Database.Generic.Entity.SqlColumns (HasSqlColumns)
import Database.Generic.Entity.ToSql (ToSqlValue, ToSqlValues)
import Database.Generic.Prelude
import GHC.Base (Symbol)
import GHC.TypeLits (KnownSymbol)

-- | An 'Entity' can be converted to/from SQL and has a primary key.
--
-- For simple Haskell records with a single data constructor and named fields
-- you can derive an instance via 'Generic' by specifing the primary key field:
-- > data Person = Person { name :: !String, age :: !Int64 }
-- >   deriving (Entity "name", Generic)
class (FromSqlValues a, HasSqlColumns a, KnownSymbol (PrimaryKey a), ToSqlValues a) => Entity a where
  type PrimaryKey a :: Symbol

  entityName         ::               EntityName
  default entityName :: Typeable a => EntityName
  entityName = EntityName $ toLower <$> showType @a

-- | 'Entity' with types of primary key in scope.
type EntityP a f b = (Entity a, HasField (PrimaryKey a) a b, ToSqlValue b)

primaryKey :: forall a f b. (EntityP a f b) => a -> b
primaryKey = let (Field _ f) = primaryKeyField @a in f

primaryKeyField :: forall a f b. EntityP a f b => Field (PrimaryKey a) a b
primaryKeyField = field @(PrimaryKey a) @a

primaryKeyFieldName :: forall a f b. (EntityP a f b) => FieldName
primaryKeyFieldName = (primaryKeyField @a).name
