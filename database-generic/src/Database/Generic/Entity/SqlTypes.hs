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

-- TODO: should be done on per-db basis.
-- TODO: docs
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
  from (H.SqlByteString b) = from b
  from (H.SqlInt64 i) = from i
  from (H.SqlInteger i) = from i
  from (H.SqlString s) = from s
  from x = error $ "SqlTypes.hs unmatched pattern on " <> show x

instance From ByteString SqlValue where
  from = SqlByteString . SqlBS

instance From Int64 SqlValue where
  from = SqlInt64

instance From Integer SqlValue where
  from = SqlInteger

instance From String SqlValue where
  from = SqlString

newtype SqlBS = SqlBS ByteString deriving (Eq, Show)

-- TODO: look at this. convert into SqlString?
instance Aeson.FromJSON SqlBS where
  parseJSON = fmap (SqlBS . read) <$> Aeson.parseJSON

instance Aeson.ToJSON SqlBS where
  toJSON = Aeson.toJSON . show
