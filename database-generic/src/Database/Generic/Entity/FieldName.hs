module Database.Generic.Entity.FieldName where

import Data.Aeson qualified as Aeson
import Database.Generic.Prelude
import Witch.Utility (over)

-- | Name of a field of an 'Entity' (column of a database collection).
--
-- For example: "foo" in 'data X = Y { foo :: Int }'.
newtype FieldName = FieldName String
  deriving (Eq, From String, Generic, Show)

instance Aeson.FromJSON FieldName

instance From FieldName String

instance IsString FieldName where
  fromString = FieldName

type HasFieldName = Typeable

-- | The name of the field, to use as the name for a database column.
--
-- See 'FieldNameTransformation' if you want to customise the name.
fieldName :: forall f a.
  (FieldNameTransformation a, HasFieldName f) => FieldName
fieldName = fieldNameT @a $ FieldName $ showType @f

-- | Implement this to have custom names for database columns.
class FieldNameTransformation a where
  fieldNameT :: FieldName -> FieldName

instance {-# OVERLAPPABLE #-} FieldNameTransformation a where
  fieldNameT = over showTypeT
