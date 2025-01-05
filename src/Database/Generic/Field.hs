module Database.Generic.Field where

import Database.Generic.Prelude
import Database.Generic.Entity.ToSql (showType')

-- | Value representing a field of a data type.
data Field f a b = (HasField f a b, Typeable f) => Field (Proxy f)

field :: forall f a b. (HasField f a b, Typeable f) => Field f a b
field = Field (Proxy :: Proxy f)

data FieldE = forall f a b. FieldE (Field f a b)

fieldE :: forall f a b. (HasField f a b, Typeable f) => FieldE
fieldE = FieldE $ field @f @a @b

class FieldType a where
  fieldType :: a -> String

instance Typeable f => FieldType (Proxy f) where
  fieldType _ = showType' @f

instance FieldType (Field f a b) where
  fieldType (Field p) = fieldType p

instance FieldType FieldE where
  fieldType (FieldE f) = fieldType f
