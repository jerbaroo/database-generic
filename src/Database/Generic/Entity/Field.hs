module Database.Generic.Entity.Field where

import Database.Generic.Entity.FieldName (FieldName, HasFieldName(..))
import Database.Generic.Prelude

-- | Value-level representation of field 'f' of type 'b' belonging to 'a'.
data Field f a b where
  Field
    :: HasField f a b
    => { name :: FieldName, get :: a -> b }
    -> Field f a b

-- | Construct a 'Field'.
field :: forall f a b. (HasField f a b, HasFieldName f) => Field f a b
field = Field (fieldName @f) (getField @f)
