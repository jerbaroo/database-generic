module Database.Generic.Field where

import Database.Generic.Prelude

-- | Types that have a single named field.
class HasFieldName f where
  fieldName :: f -> String

-- | Value-level representation of a named-field of a data type.
data Field f a b where
  Field :: (HasField f a b) => String -> (a -> b) -> Field f a b

-- | Construct a 'Field'.
field :: forall f a b. (HasField f a b, Typeable f) => Field f a b
field = Field (showType' @f) (getField @f)

instance HasFieldName (Field f a b) where
  fieldName (Field name _) = name
