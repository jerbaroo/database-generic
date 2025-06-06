{-# LANGUAGE UndecidableInstances #-}

module Database.Generic.Entity.PrimaryKey where

import Database.Generic.Entity.Field (Field(..), field)
import Database.Generic.Entity.FieldName (FieldName, HasFieldName)
import Database.Generic.Prelude
import GHC.Base (Symbol)

-- | Types with a primary key.
class PrimaryKey a => HasPrimaryKey a where
  -- | Return a 'Field' for the primary key.
  primaryKeyField :: forall b. HasField (PKField a) a b => Field (PKField a) a b
  primaryKeyField = field @(PKField a) @a

primaryKey :: forall a f b. HasPrimaryKeyP a f b => a -> b
primaryKey = let (Field _ f) = primaryKeyField @a in f

primaryKeyFieldName :: forall a f b. (HasPrimaryKeyP a f b) => FieldName
primaryKeyFieldName = (primaryKeyField @a).name

instance PrimaryKey a => HasPrimaryKey a

-- | 'HasPrimaryKey' with additional types of primary key in scope.
type HasPrimaryKeyP a f b = (HasField (PKField a) a b, HasPrimaryKey a)

-- TODO: can we merge this with 'HasPrimaryKey'?
-- | Exists for convenience for library users. Example usage:
-- > data Person = Person { age :: Int, name :: String }
-- > deriving PrimaryKey via PK "name" Person
class HasFieldName (PKField a) => PrimaryKey a where
  type PKField a :: Symbol

newtype PK (f :: Symbol) a = PK a

instance HasFieldName f => PrimaryKey (PK f a) where
  type PKField (PK f a) = f
