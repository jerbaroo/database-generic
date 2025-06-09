-- TODO review
module Database.Generic.Entity.SqlTypes where

import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Database.Generic.Prelude
import Database.HDBC qualified as H
import Prelude (read)

-- | Names of types in SQL.
data SqlType
  = SqlBigInt
  | SqlVarChar
  deriving (Eq, Generic, Show)

instance Aeson.FromJSON SqlType
instance Aeson.ToJSON   SqlType

-- | Types that have a corresponding SQL type.
class HasSqlType a where
  sqlType :: SqlType

instance HasSqlType Int64 where
  sqlType = SqlBigInt

instance HasSqlType String where
  sqlType = SqlVarChar

data SqlValue
  = SqlByteString !SqlBS
  | SqlInt64      !Int64
  | SqlInteger    !Integer
  | SqlString     !String
  deriving (Eq, Generic, Show)

instance Aeson.FromJSON SqlValue
instance Aeson.ToJSON   SqlValue

instance From H.SqlValue SqlValue where
  from (H.SqlInt64 s) = from s
  from (H.SqlString s) = from s
  from x = error $ "SqlTypes.hs unmatched pattern on " <> show x

instance From Int64 SqlValue where
  from = SqlInt64

instance From String SqlValue where
  from = SqlString

newtype SqlBS = SqlBS ByteString deriving (Eq, Show)

instance Aeson.FromJSON SqlBS where
  parseJSON = fmap (SqlBS . read) <$> Aeson.parseJSON

instance Aeson.ToJSON SqlBS where
  toJSON = Aeson.toJSON . show
