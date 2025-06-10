module Database.Generic.Entity.SqlTypes where

import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Database.Generic.Prelude
import Database.HDBC qualified as H
import Prelude (read)

data DbType
  = DbInt64
  | DbString
  deriving (Eq, Generic, Show)

instance Aeson.FromJSON DbType
instance Aeson.ToJSON   DbType

class HasDbType a where
  dbType :: DbType

instance HasDbType Int64 where
  dbType = DbInt64

instance HasDbType String where
  dbType = DbString

data DbValue
  -- = SqlByteString !SqlBS
  = DbVInt64  !Int64
  | DbVString !String
  deriving (Eq, Generic, Show)

instance Aeson.FromJSON DbValue
instance Aeson.ToJSON   DbValue

-- -- TODO client shouldn't be HDBC-specific
-- instance From H.SqlValue DbValue where
--   from (H.SqlByteString b) = from b
--   from (H.SqlInt64 i) = from i
--   from (H.SqlInteger i) = from i
--   from (H.SqlString s) = from s
--   from x = error $ "SqlTypes.hs unmatched pattern on " <> show x

-- instance From ByteString SqlValue where
--   from = SqlByteString . SqlBS

-- instance From Int64 SqlValue where
--   from = SqlInt64

-- instance From Integer SqlValue where
--   from = SqlInteger

-- instance From String SqlValue where
--   from = SqlString

-- newtype SqlBS = SqlBS ByteString deriving (Eq, Show)

-- -- TODO: look at this. convert into SqlString?
-- instance Aeson.FromJSON SqlBS where
--   parseJSON = fmap (SqlBS . read) <$> Aeson.parseJSON

-- instance Aeson.ToJSON SqlBS where
--   toJSON = Aeson.toJSON . show
