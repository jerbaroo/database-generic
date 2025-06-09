module Database.Generic.Entity.SqlTypes (SqlType(..), module X) where

import Data.Aeson qualified as Aeson
import Database.Generic.Prelude
import Database.HDBC as X (SqlTypeId(..), SqlValue(..))
import Prelude (read)

instance Aeson.FromJSON SqlValue where
  parseJSON = fmap read <$> Aeson.parseJSON @String

instance Aeson.ToJSON SqlValue where
  toJSON = Aeson.toJSON . show

deriving instance Read SqlValue

-- TODO newtype for SqlValue

-- | Types that have a corresponding SQL type.
class SqlType a where
  sqlType :: SqlTypeId

instance SqlType Int64 where
  sqlType = SqlBigIntT

instance SqlType String where
  sqlType = SqlLongVarCharT
