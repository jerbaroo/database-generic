module Database.Generic.Statement.Fields where

import Database.Generic.Field (Field(..))
import Database.Generic.Statement.Returning (NowReturning, Returning)
import Database.Generic.Prelude

-- | Fields of 'a' that can be parsed into a 'b'.
class FieldsOf f a b | f -> a, f -> b where
  fieldNames :: f -> [String]

instance FieldsOf (Field fa a b) a b where
  fieldNames fb = [fb.name]

instance FieldsOf (Field fa a b, Field fc a c) a (b, c) where
  fieldNames (fb, fc) = [fb.name, fc.name]

-- | Statements for which a subset of fields can be returned.
class ReturningFields s where
  fields :: forall p b. (FieldsOf p (Returning s) b)
    => s -> p -> NowReturning s b

infixl 4 ==>

(==>) :: forall s p b.
  (FieldsOf p (Returning s) b, ReturningFields s)
  => s -> p -> NowReturning s b
(==>) = fields
