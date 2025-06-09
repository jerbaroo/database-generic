module Database.Generic.Statement.Where where

import Data.Aeson qualified as Aeson
import Database.Generic.Entity (Entity, Entity')
import Database.Generic.Entity.FieldName (FieldName, fieldName)
import Database.Generic.Entity.PrimaryKey (primaryKeyFieldName)
import Database.Generic.Entity.SqlTypes (SqlValue(..))
import Database.Generic.Entity.ToSql (ToSqlValue(..))
import Database.Generic.Prelude
import Database.Generic.Serialize (Serialize(..))

-- | Condition to filter values of type 'a'.
data Where' where
  And    :: !Where'    -> !Where'   -> Where'
  Equals :: !FieldName -> !SqlValue -> Where'
  IsNull :: !FieldName -> !Bool     -> Where'
  deriving (Eq, Generic, Show)

instance Aeson.FromJSON Where'

instance Serialize SqlValue db => Serialize Where' db where
  serialize (And a b) =
    "(" <> serialize @_ @db a <> " AND " <> serialize @_ @db b <> ")"
  serialize (Equals fName value) = from fName <> "=" <> serialize @_ @db value
  serialize (IsNull fName is) =
    from fName <> " IS " <> if is then "" else "NOT " <> "NULL"

idEquals :: forall a f b. Entity' a f b => b -> Where'
idEquals b = Equals (primaryKeyFieldName @a) (toSqlValue b)

isNull :: forall a f. Entity a f => Where'
isNull = IsNull (fieldName @f) True

isNotNull :: forall a f. Entity a f => Where'
isNotNull = IsNull (fieldName @f) False

class Whereable s a where
  where' :: s -> Where' -> s
