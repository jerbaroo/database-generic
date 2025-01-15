module Database.Generic.Field where

import Database.Generic.Prelude

type FieldName = String

-- | Value-level representation of field 'f' of type 'b' belonging to 'a'.
data Field f a b where
  Field
    :: HasField f a b
    => { name :: FieldName, get :: a -> b }
    -> Field f a b

-- | Construct a 'Field'.
field :: forall f a b. (HasField f a b, HasFieldName f) => Field f a b
field = Field (fieldName @f) (getField @f)

-- | Types that represent a named-field.
class HasFieldName f where
  fieldName :: FieldName

instance {-# OVERLAPPABLE #-} Typeable f => HasFieldName f where
  fieldName = showType' @f
