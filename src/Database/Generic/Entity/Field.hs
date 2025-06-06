module Database.Generic.Entity.Field where

import Database.Generic.Entity.FieldName (FieldName, fieldName)
import Database.Generic.Prelude
import GHC.Generics (Generic)

-- | Value-level representation of field 'f' of type 'b' belonging to 'a'.
data Field f a b = Field
  { name :: !FieldName
  , get  :: !(a -> b)
  }
  deriving Generic

-- | Construct a 'Field'.
field :: forall f a b. (HasField f a b, Typeable f) => Field f a b
field = Field (fieldName @f) (getField @f)
