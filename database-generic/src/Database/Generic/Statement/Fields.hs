module Database.Generic.Statement.Fields where

import Data.Aeson qualified as Aeson
import Database.Generic.Entity.FieldName (FieldName, HasFieldName, fieldName)
import Database.Generic.Prelude
import Database.Generic.Statement.Order (Order(..))
import Database.Generic.Serialize (Serialize(..))
import Witch qualified as W
import Database.Generic.Statement.Type (Cons)

-- | Named fields in a statement.
data Fields = All | Some ![FieldName]
  deriving (Eq, Generic, Show)

instance Aeson.FromJSON Fields

instance Serialize Fields db where
  serialize All       = "*"
  serialize (Some cs) = intercalate ", " $ W.from <$> cs

-- | Named fields to order by in a statement.
newtype OrderedFields = OrderedFields [(FieldName, Order)]
  deriving (Aeson.FromJSON, Eq, Generic, Semigroup, Show)

instance Serialize Order db => Serialize OrderedFields db where
  serialize (OrderedFields fs) = intercalate ", " $ fs <&> \(fn, o) ->
    from fn <> " " <> serialize @_ @db o

-- | Fields 'fs' of 'a' that can be parsed into a 'b'.
class FieldsOf fs a b | fs -> a, fs -> b where
  -- | The names of the fields to be selected.
  fieldNames :: fs -> [FieldName] -- TODO

-- | Ordered fields 'fs' of 'a'.
class OrderedFieldsOf fs a | fs -> a where
  -- | The names of the fields to be selected.
  orderedFieldNames :: fs -> OrderedFields

-- * field - field3

-- | A named field of type 'b' belonging to 'a'.
field :: forall f a b. (HasField f a b, HasFieldName f) => Field' a '[b]
field = Field $ fieldName @f

-- | A named fields of type 'b' belonging to 'a', with ordering 'o'.
fieldOrder
  :: forall f (o :: Order) a b
  . (HasField f a b, HasFieldName f)
  => FieldOrder a '[o]
fieldOrder = Field (fieldName @f)

-- | Cons fields together.
(/\) :: Field' a '[b] -> Field' a bs -> Field' a (Cons b bs)
(/\) = FieldCons

-- | One or more named fields belonging to 'a'.
--
-- Can be used to filter results to only a subset of 'a's fields.
type Field a (bs :: [Type]) = Field' a bs

-- | One or more named fields belonging to 'a', with ordering 'o'.
--
-- This can be used to order a query of a collection of 'a's.
type FieldOrder a (os :: [Order]) = Field' a os

data Field' a x where
  Field     :: !FieldName       -> Field' a '[b]
  FieldCons :: !(Field' a '[b]) -> !(Field' a bs) -> Field' a (Cons b bs)

instance FieldsOf (Field' a '[b]) a b where
  fieldNames (Field fn) = [fn]

instance FieldsOf (Field' a '[b1, b2]) a (b1, b2) where
  fieldNames (FieldCons f fs) = fieldNames f <> fieldNames fs

instance FieldsOf (Field' a '[b1, b2, b3]) a (b1, b2, b3) where
  fieldNames (FieldCons f fs) = fieldNames f <> fieldNames fs

instance FieldsOf (Field' a '[b1, b2, b3, b4]) a (b1, b2, b3, b4) where
  fieldNames (FieldCons f fs) = fieldNames f <> fieldNames fs

instance OrderedFieldsOf (Field' a '[Asc]) a where
  orderedFieldNames (Field fn) = OrderedFields [(fn, Asc)]

instance OrderedFieldsOf (Field' a '[Desc]) a where
  orderedFieldNames (Field fn) = OrderedFields [(fn, Desc)]

-- instance OrderedFieldsOf (Field' a '[o1, o2]) a where
--   orderedFieldNames (FieldCons f fs) =
--     orderedFieldNames f <> orderedFieldNames fs
