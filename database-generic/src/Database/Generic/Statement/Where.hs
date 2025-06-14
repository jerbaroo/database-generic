{-# LANGUAGE UndecidableInstances #-}

module Database.Generic.Statement.Where where

import Data.Aeson qualified as Aeson
import Database.Generic.Entity.FieldName (FieldName, fieldName, HasFieldName)
import Database.Generic.Entity.PrimaryKey (PrimaryKey', primaryKeyFieldName)
import Database.Generic.Entity.SqlTypes (DbValue)
import Database.Generic.Entity.ToSql (ToDbValue(..))
import Database.Generic.Prelude
import Database.Generic.Serialize (Serialize(..))

-- | Condition to filter values of type 'a'.
data Where' where
  And    :: !Where'    -> !Where'   -> Where'
  Equals :: !FieldName -> !DbValue -> Where'
  IsNull :: !FieldName -> !Bool     -> Where'
  deriving (Eq, Generic, Show)

instance Aeson.FromJSON Where'

instance Serialize DbValue db => Serialize Where' db where
  serialize (And a b) =
    "(" <> serialize @_ @db a <> " AND " <> serialize @_ @db b <> ")"
  serialize (Equals fName value) = from fName <> "=" <> serialize @_ @db value
  serialize (IsNull fName is) =
    from fName <> " IS " <> if is then "" else "NOT " <> "NULL"

idEquals :: forall a f b. (PrimaryKey' a f b, ToDbValue b) => b -> Where'
idEquals b = Equals (primaryKeyFieldName @a) (toDbValue b)

isNull :: forall f a b. (HasField f a (Maybe b), HasFieldName f) => Where'
isNull = IsNull (fieldName @f) True

isNotNull :: forall f a b. (HasField f a (Maybe b), HasFieldName f) => Where'
isNotNull = IsNull (fieldName @f) False

class Whereable s a where
  where' :: s -> Where' -> s
