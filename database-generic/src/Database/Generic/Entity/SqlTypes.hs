{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Generic.Entity.SqlTypes where

import Data.Aeson qualified as Aeson
-- import Data.ByteString (ByteString)
import Database.Generic.Prelude
-- import Database.HDBC qualified as H
-- import Prelude (read)

data DbT f
  = DbInt64  !(Eval f Int64)
  | DbString !(Eval f String)
  deriving Generic

deriving instance Eq   (DbT Id)
deriving instance Eq   (DbT Unit)
deriving instance Show (DbT Id)
deriving instance Show (DbT Unit)

data Id
data Unit

type Eval :: forall x a b. (x :: Type) -> (a :: Type) -> (b :: Type)
type family Eval f a where
  Eval Id   a = a
  Eval Unit _ = ()

type DbValue = DbT Id

-- TODO test this
-- instance Aeson.FromJSON DbValue
-- instance Aeson.ToJSON   DbValue

type DbType = DbT Unit

class HasDbType a where
  dbType :: DbType

instance HasDbType Int64 where
  dbType = DbInt64 ()

instance HasDbType String where
  dbType = DbString ()
