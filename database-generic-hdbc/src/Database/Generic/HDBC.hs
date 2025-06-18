module Database.Generic.HDBC where

import Database.Generic.Entity.DbTypes (Bytes(..), DbT(..), DbValueN)
import Database.Generic.Prelude
import Database.HDBC qualified as HDBC
import Witch (From(from))

instance From HDBC.SqlValue DbValueN where
  from (HDBC.SqlBool       b) = Just $ DbBool b
  from (HDBC.SqlString     s) = Just $ DbString s
  from (HDBC.SqlByteString b) = Just $ DbBytes $ Bytes b
  from (HDBC.SqlInt64      i) = Just $ DbInt64 i
  from (HDBC.SqlInteger    i) = Just $ DbInteger i
  from HDBC.SqlNull           = Nothing
  from x = error $ "Error 'From HDBC.SqlValue DbValue': " <> show x
