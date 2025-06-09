module Database.Generic.Entity.SqlColumns where

import Database.Generic.Entity.FieldName (FieldName, HasFieldNames(..))
import Database.Generic.Entity.SqlColumnTypes (HasSqlColumnTypes(..))
import Database.Generic.Entity.SqlTypes (SqlType)
import Database.Generic.Prelude

-- | Name of field, and corresponding SQL type, for each field of 'a'.
class (HasFieldNames a, HasSqlColumnTypes a) => HasSqlColumns a where
  sqlColumns :: [(FieldName, SqlType)]

instance (HasFieldNames a, HasSqlColumnTypes a) => HasSqlColumns a where
  sqlColumns = zip (fieldNames @a) (sqlColumnTypes @a)
