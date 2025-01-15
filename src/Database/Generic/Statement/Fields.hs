module Database.Generic.Statement.Fields where

import Database.Generic.Entity.Field (Field(..))
import Database.Generic.Entity.FieldName (FieldName)
import Database.Generic.Statement.Returning (NowReturning, Returning)
import Database.Generic.Prelude

-- | Fields of 'a' that can be parsed into a 'b'.
class FieldsOf f a b | f -> a, f -> b where
  fieldNames :: f -> [FieldName]

instance FieldsOf (Field fa a b) a b where
  fieldNames fb = [fb.name]

instance FieldsOf (Field fa a b, Field fc a c) a (b, c) where
  fieldNames (fb, fc) = [fb.name, fc.name]

-- | Statements for which a subset of fields can be returned.
class ReturningFields s where
  fields :: forall f b. (FieldsOf f (Returning s) b)
    => s                -- ^ The original statement.
    -> f                -- ^ Fields to select, which can be parsed into a 'b'.
    -> NowReturning s b -- ^ A statement now returning values of type 'b'.

infixl 4 ==>

(==>) :: forall s p b.
  (FieldsOf p (Returning s) b, ReturningFields s)
  => s -> p -> NowReturning s b
(==>) = fields
