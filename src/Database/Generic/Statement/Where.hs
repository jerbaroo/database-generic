module Database.Generic.Statement.Where where

import Database.Generic.Entity (Entity)
import Database.Generic.Entity qualified as Entity
import Database.Generic.Entity.SqlTypes (SqlValue(..))
import Database.Generic.Entity.ToSql (ToSqlValue(..))
import Database.Generic.Entity.FieldName (FieldName, HasFieldName(..))
import Database.Generic.Prelude
import Database.Generic.Serialize (Serialize(..))

-- | Condition to filter values of type 'a'.
data Where a where
  And    :: !(Where a) -> !(Where a) -> Where a
  Equals :: !FieldName -> !SqlValue  -> Where a
  IsNull :: !FieldName -> !Bool      -> Where a

instance Serialize SqlValue db => Serialize (Where a) db where
  serialize (And a b) =
    "(" <> serialize @_ @db a <> " AND " <> serialize @_ @db b <> ")"
  serialize (Equals fName value) = from fName <> " = " <> serialize @_ @db value
  serialize (IsNull fName is) =
    from fName <> " IS " <> if is then "" else "NOT " <> "NULL"

idEquals :: forall a f b.
  (Entity f a, HasField f a b, ToSqlValue b) => b -> Where a
idEquals b = Equals (Entity.primaryKeyFieldName @a) (toSqlValue b)

isNull :: forall f a b.
  (Entity f a, HasField f a b, HasFieldName f) => Where a
isNull = IsNull (fieldName @f) True

isNotNull :: forall f a b.
  (Entity f a, HasField f a b, HasFieldName f) => Where a
isNotNull = IsNull (fieldName @f) False

class Whereable s a where
  where' :: s -> Where a -> s
