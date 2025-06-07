module Database.Generic.Statement.Fields where

import Database.Generic.Entity.Field (Field(..))
import Database.Generic.Entity.FieldName (FieldName)
import Database.Generic.Prelude
import Database.Generic.Serialize (Serialize(..))
import Database.Generic.Statement.Returning (ModifyReturning, Returning)
import Witch qualified as W

data Fields = All | Some ![FieldName]

instance Serialize Fields db where
  serialize All       = "*"
  serialize (Some cs) = intercalate ", " $ W.from <$> cs

-- | Fields 'fs' of 'a' that can be parsed into 'b'.
class FieldsOf fs a b | fs -> a, fs -> b where
  -- | The names of the fields to be selected.
  fieldNames :: fs -> [FieldName]

instance FieldsOf (Field fa a b) a b where
  fieldNames fb = [fb.name]

instance FieldsOf (Field fa a b, Field fc a c) a (b, c) where
  fieldNames (fb, fc) = [fb.name, fc.name]

-- | Statements which can be modified to return a subset of fields.
class ReturnFields s where
  fields :: forall f b. (FieldsOf f (Returning s) b)
    => s                   -- ^ The original statement.
    -> f                   -- ^ Fields to select, which can be parsed into a 'b'.
    -> ModifyReturning s b -- ^ A statement now returning values of type 'b'.

infixl 4 ==>

(==>) :: forall s p b.
  (FieldsOf p (Returning s) b, ReturnFields s)
  => s -> p -> ModifyReturning s b
(==>) = fields
