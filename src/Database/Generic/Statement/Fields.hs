module Database.Generic.Statement.Fields where

import Database.Generic.Field (Field, fieldName)
import Database.Generic.Statement.Returning (ReturnType, Returning)
import Database.Generic.Prelude

-- | Fields of 'a' that return the composite type 'b'.
class FieldsOf f a b | f -> a, f -> b where
  fieldNames :: f -> [String]

instance FieldsOf (Field fa a b) a b where
  fieldNames fb = [fieldName fb]

instance FieldsOf (Field fa a b, Field fc a c) a (b, c) where
  fieldNames (fb, fc) = [fieldName fb, fieldName fc]

class SelectFields s where
  fields :: forall p b r. (FieldsOf p (ReturnType r) b) =>
    s r -> p -> s (Returning r b)
