{-# LANGUAGE UndecidableInstances #-}

module Database.Generic.Entity.PrimaryKey where

import Database.Generic.Entity.Field (Field(..), field)
import Database.Generic.Entity.FieldName (FieldName, HasFieldName)
import Database.Generic.Prelude
import GHC.Base (Symbol)
import GHC.TypeLits (KnownSymbol)

-- | Types with a primary key.
class HasPKField a => HasPrimaryKey a where
  -- | Return a 'Field' for the primary key.
  primaryKeyField :: forall b. HasField (PKField a) a b => Field (PKField a) a b
  primaryKeyField = field @(PKField a) @a

instance HasPKField a => HasPrimaryKey a

-- | 'HasPrimaryKey' with additional types of primary key in scope.
type HasPrimaryKeyP a f b = (HasField (PKField a) a b, HasPrimaryKey a)

primaryKey :: forall a f b. HasPrimaryKeyP a f b => a -> b
primaryKey = let (Field _ f) = primaryKeyField @a in f

primaryKeyFieldName :: forall a f b. (HasPrimaryKeyP a f b) => FieldName
primaryKeyFieldName = (primaryKeyField @a).name

-- | Types with a named field to use as primary key.
class (HasFieldName (PKField a), KnownSymbol (PKField a)) => HasPKField a where
  type PKField a :: Symbol

-- | 'HasPKField' can be derived via this newtype. For example:
-- > deriving HasPKField via PK "name" Person
newtype PK (f :: Symbol) a = PK a

instance (HasFieldName f, KnownSymbol f) => HasPKField (PK f a) where
  type PKField (PK f a) = f
