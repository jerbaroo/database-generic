module Database.Generic.Statement.Fields where

import Database.Generic.Entity.Field (Field(..), field)
import Database.Generic.Entity.FieldName (FieldName)
import Database.Generic.Prelude
import Database.Generic.Serialize (Serialize(..))
import Witch qualified as W

data Fields = All | Some ![FieldName]
  deriving (Eq, Show)

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
