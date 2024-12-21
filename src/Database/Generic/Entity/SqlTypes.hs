module Database.Generic.Entity.SqlTypes where

import Database.Generic.Prelude
import Database.HDBC qualified as HDBC

type SqlValue = HDBC.SqlValue

data SqlType
  = SqlInt64
  | SqlString
  deriving Show
