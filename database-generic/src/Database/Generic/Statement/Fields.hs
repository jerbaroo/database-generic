module Database.Generic.Statement.Fields where

import Data.Aeson qualified as Aeson
import Database.Generic.Entity.FieldName (FieldName, HasFieldName, fieldName)
import Database.Generic.Prelude
import Database.Generic.Statement.Order (Order(..))
import Database.Generic.Statement.Type (List(..))
import Database.Generic.Serialize (Serialize(..))
import Witch qualified as W

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
  fieldNames :: fs -> [FieldName]

-- | Ordered fields 'fs' of 'a'.
class OrderedFieldsOf fs a | fs -> a where
  -- | The names of the fields to be selected.
  orderedFieldNames :: fs -> OrderedFields

-- * field - field3

-- | A named field of type 'b' belonging to 'a'.
field :: forall f a b. (HasField f a b, HasFieldName f) => Field' a (One b)
field = Field $ fieldName @f

-- | A named fields of type 'b' belonging to 'a', with ordering 'o'.
order
  :: forall f (o :: Order) a b
  . (HasField f a b, HasFieldName f)
  => FieldOrder a (One o)
order = Field (fieldName @f)

-- | Cons fields together.
(/\) :: Field' a (One b) -> Field' a bs -> Field' a (L b bs)
(/\) = FieldCons

-- | One or more named fields belonging to 'a'.
--
-- Can be used to filter results to only a subset of 'a's fields.
type Field a (bs :: List Type) = Field' a bs

-- | One or more named fields belonging to 'a', with ordering 'o'.
--
-- This can be used to order a query of a collection of 'a's.
type FieldOrder a (os :: List Order) = Field' a os

data Field' a x where
  Field     :: !FieldName           -> Field' a (One b)
  FieldCons :: !(Field' a (One b)) -> !(Field' a bs) -> Field' a (L b bs)

-- TODO the following code should avoid an instance per list length

instance FieldsOf (Field' a (One b)) a b where
  fieldNames (Field fn) = [fn]

instance FieldsOf (Field' a (L b1 (One b2))) a (b1, b2) where
  fieldNames (FieldCons f fs) = fieldNames f <> fieldNames fs

instance FieldsOf (Field' a (L b1 (L b2 (One b3)))) a (b1, b2, b3) where
  fieldNames (FieldCons f fs) = fieldNames f <> fieldNames fs

instance FieldsOf (Field' a (L b1 (L b2 (L b3 (One b4))))) a (b1, b2, b3, b4) where
  fieldNames (FieldCons f fs) = fieldNames f <> fieldNames fs

class    SingOrder o    where singOrder :: Order
instance SingOrder Asc  where singOrder =  Asc
instance SingOrder Desc where singOrder =  Desc

-- TODO the following code should avoid an instance per list length

instance SingOrder o => OrderedFieldsOf (Field' a (One o)) a where
  orderedFieldNames (Field fn) = OrderedFields [(fn, singOrder @o)]

instance (SingOrder o1, SingOrder o2) => OrderedFieldsOf (Field' a (L o1 (One o2)) ) a where
  orderedFieldNames (FieldCons (Field f1) (Field f2)) = OrderedFields [(f1, singOrder @o1), (f2, singOrder @o2)]

instance (SingOrder o1, SingOrder o2, SingOrder o3) => OrderedFieldsOf (Field' a (L o1 (L o2 (One o3))) ) a where
  orderedFieldNames (FieldCons (Field f1) (FieldCons (Field f2) (Field f3))) = OrderedFields [(f1, singOrder @o1), (f2, singOrder @o2), (f3, singOrder @o3)]
