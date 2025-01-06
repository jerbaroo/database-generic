module Database.Generic.Statement.Fields where

import Database.Generic.Field (Field, fieldType)
import Database.Generic.Statement.Returning (Returning(..), ReturningType)
import Database.Generic.Prelude

-- | Fields of 'a' that return the composite type 'b'.
class FieldsOf f a b | f -> a, f -> b where
  fieldNames :: f -> [String]

instance FieldsOf (Field fa a b) a b where
  fieldNames fb = [fieldType fb]

instance FieldsOf (Field fa a b, Field fc a c) a (b, c) where
  fieldNames (fb, fc) = [fieldType fb, fieldType fc]

type ReturningFields :: forall r1 b r2. r1 -> b -> r2
type family ReturningFields r b where
  ReturningFields (MaybeOne _) b = MaybeOne b

class SelectFields s where
  fields :: forall p b r. (FieldsOf p (ReturningType r) b) =>
    s r -> p -> s (ReturningFields r b)
