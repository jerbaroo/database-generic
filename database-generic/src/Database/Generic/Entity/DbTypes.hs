{-# LANGUAGE DeriveAnyClass #-}

module Database.Generic.Entity.DbTypes where

import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Database.Generic.Prelude

class DbT a where
  type DbTUnit a :: Type
  type DbTUnit a :: Type
data DbT f
  = DbBool    !(F f Bool)
  | DbBytes   !(F f ByteString)
  | DbInt64   !(F f Int64)
  | DbInteger !(F f Integer)
  | DbString  !(F f String)
  deriving Generic

type F :: forall f a b. (f :: Type) -> (a :: Type) -> (b :: Type)
type family F f a where
  F Id   a = a
  F Unit _ = Unit

-- | A database type.
type DbType = DbT Unit
data Unit   = Unit deriving (Aeson.FromJSON, Eq, Generic, Show)

-- | Slim wrapper over 'DbType' to allow for a nullable flag.
data DbTypeN = DbTypeN !Bool !DbType
  deriving (Aeson.FromJSON, Eq, Generic, Show)

deriving instance Aeson.FromJSON (DbT Unit)
deriving instance Eq             (DbT Unit)
deriving instance Show           (DbT Unit)

-- TODO should have db as a type parameter?
class HasDbType a where
  dbType :: DbTypeN

instance HasDbType Bool where
  dbType = DbTypeN False $ DbBool Unit

instance HasDbType Int64 where
  dbType = DbTypeN False $ DbInt64 Unit

instance HasDbType String where
  dbType = DbTypeN False $ DbString Unit

instance HasDbType a => HasDbType (Maybe a) where
  dbType = case dbType @a of
    DbTypeN False t -> DbTypeN True t -- Becomes nullable.
    x               -> x

-- | A database value.
type DbValue = DbT Id
data Id

-- | Slim wrapper over 'DbValue' to allow for nullable values.
type DbValueN = Maybe DbValue

deriving instance Aeson.FromJSON (DbT Id)
deriving instance Aeson.ToJSON   (DbT Id)
deriving instance Eq             (DbT Id)
deriving instance Show           (DbT Id)
