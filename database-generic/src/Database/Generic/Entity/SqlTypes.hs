{-# LANGUAGE DeriveAnyClass #-}

module Database.Generic.Entity.SqlTypes where

import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Database.Generic.Entity.FromSql (FromDbValues(..))
import Database.Generic.Prelude
import Database.HDBC qualified as HDBC

data DbT f
  = DbBytes   !(F f Bytes)
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

deriving instance Aeson.FromJSON (DbT Unit)
deriving instance Eq             (DbT Unit)
deriving instance Show           (DbT Unit)

class HasDbType dbt a where
  dbType :: dbt

instance HasDbType DbType Int64 where
  dbType = DbInt64 Unit

instance HasDbType DbType String where
  dbType = DbString Unit

data Id
type DbValue = DbT Id

deriving instance Aeson.FromJSON (DbT Id)
deriving instance Aeson.ToJSON   (DbT Id)
deriving instance Eq             (DbT Id)
deriving instance Show           (DbT Id)

instance From Int64 DbValue  where from = DbInt64
instance From String DbValue where from = DbString

instance FromDbValues DbValue Int64 where
  fromDbValues [DbInt64   i] = i
  fromDbValues [DbInteger i] = unsafeFrom i
  fromDbValues x = error $ "Error constructing Int64 from " <> show x

instance FromDbValues DbValue String where
  fromDbValues [DbBytes  b] = from b
  fromDbValues [DbString s] = s
  fromDbValues x = error $ "Error constructing Int64 from " <> show x

-- TODO move to own module
instance From HDBC.SqlValue DbValue where
  from (HDBC.SqlString     s) = DbString s
  from (HDBC.SqlByteString b) = DbBytes $ Bytes b
  from (HDBC.SqlInt64      i) = DbInt64 i
  from (HDBC.SqlInteger    i) = DbInteger i
  from x = error $ "Error 'From HDBC.SqlValue DbValue': " <> show x

newtype Bytes = Bytes ByteString deriving (Eq, Show)

instance Aeson.FromJSON Bytes where
  parseJSON = fmap (from @String) <$> Aeson.parseJSON

instance Aeson.ToJSON Bytes where
  toJSON (Bytes b)= Aeson.toJSON $ BS.unpack b

instance From Bytes String where
  from (Bytes b) = BS.unpack b

instance From String Bytes where
  from = Bytes . BS.pack
