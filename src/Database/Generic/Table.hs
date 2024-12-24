module Database.Generic.Table where

import Database.Generic.Prelude
import Database.Generic.Serialize (Serialize(serialize))

newtype TableName = TableName String

instance Serialize TableName db where
  serialize (TableName s) = s
