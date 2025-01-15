module Database.Generic.Entity.SqlTypes (SqlType(..), module X) where

import Database.HDBC as X (SqlTypeId(..), SqlValue(..))
import Database.Generic.Prelude

-- | Types that have a corresponding SQL type.
class SqlType a where
  sqlType :: SqlTypeId

instance SqlType Int64 where
  sqlType = SqlBigIntT

instance SqlType String where
  sqlType = SqlLongVarCharT
