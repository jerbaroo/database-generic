module Database.Generic.Table where

import Database.Generic.Prelude
import Database.Generic.Serialize (Serialize(serialize))

newtype ColumnName = ColumnName String

instance From ColumnName String

newtype TableName = TableName String

instance From TableName String

instance Serialize TableName db where
  serialize (TableName s) = s
