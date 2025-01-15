module Database.Generic.Entity.SqlFields where

import Database.Generic.Entity.FieldName (FieldName, HasFieldNames(..))
import Database.Generic.Entity.SqlFieldTypes (HasSqlFieldTypes(..))
import Database.Generic.Entity.SqlTypes (SqlTypeId)
import Database.Generic.Prelude

-- | Name and SQL type for each field of 'a'.
class (HasFieldNames a, HasSqlFieldTypes a) => HasSqlFields a where
  sqlFields :: [(FieldName, SqlTypeId)]

instance (HasFieldNames a, HasSqlFieldTypes a) => HasSqlFields a where
  sqlFields = zip (fieldNames @a) (sqlFieldTypes @a)
