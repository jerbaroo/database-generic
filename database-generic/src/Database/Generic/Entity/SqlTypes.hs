{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances  #-}

module Database.Generic.Entity.SqlTypes where

import Data.Aeson qualified as Aeson
-- import Data.ByteString (ByteString)
import Database.Generic.Prelude
-- import Database.HDBC qualified as H
-- import Prelude (read)

data DbT f
  = DbInt64  !(F f Int64)
  | DbString !(F f String)
  deriving Generic

-- TODO try defunctionalisation
type F :: forall f a b. (f :: Type) -> (a :: Type) -> (b :: Type)
type family F f a where
  F Id   a = a
  F Unit _ = Unit

data Unit   = Unit deriving (Aeson.FromJSON, Eq, Generic, Show)
type DbType = DbT Unit

deriving instance Aeson.FromJSON (DbT Unit)
deriving instance Eq             (DbT Unit)
deriving instance Show           (DbT Unit)

class HasDbType a where
  dbType :: DbType

instance HasDbType Int64 where
  dbType = DbInt64 Unit

instance HasDbType String where
  dbType = DbString Unit

data Id
type DbValue = DbT Id

deriving instance Aeson.FromJSON (DbT Id)
deriving instance Aeson.ToJSON   (DbT Id)
deriving instance Eq             (DbT Id)
deriving instance Show           (DbT Id)

-- TODO test this
-- instance Aeson.FromJSON DbValue
-- instance Aeson.ToJSON   DbValue
