{-# LANGUAGE DeriveAnyClass #-}

module Database.Generic.Entity.DbTypes where

import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Database.Generic.Prelude

-- | A primitive database type.
data DbT f
  = DbBool    !(F f Bool)
  | DbBytes   !(F f Bytes)
  | DbInt64   !(F f Int64)
  | DbInteger !(F f Integer)
  | DbString  !(F f String)
  deriving Generic

type F :: forall f a b. (f :: Type) -> (a :: Type) -> (b :: Type)
type family F f a where
  F Id   a = a
  F Unit _ = Unit

data Unit   = Unit deriving (Aeson.FromJSON, Eq, Generic, Show)
type DbType = DbT Unit

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

data Id
type DbValue = DbT Id

-- | Slim wrapper over 'DbValue' to allow for nullable values.
type DbValueN = Maybe DbValue

deriving instance Aeson.FromJSON (DbT Id)
deriving instance Aeson.ToJSON   (DbT Id)
deriving instance Eq             (DbT Id)
deriving instance Show           (DbT Id)

-- instance From Bool   DbValue where from = DbBool
-- instance From Int64  DbValue where from = DbInt64
-- instance From String DbValue where from = DbString

newtype Bytes = Bytes ByteString deriving (Eq, Show)

instance Aeson.FromJSON Bytes where
  parseJSON = fmap (Bytes . BS.pack) <$> Aeson.parseJSON

instance Aeson.ToJSON Bytes where
  toJSON (Bytes b)= Aeson.toJSON $ BS.unpack b

-- instance From Bytes String where
--   from (Bytes b) = BS.unpack b

-- instance From String Bytes where
--   from = Bytes . BS.pack
