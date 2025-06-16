module Database.Generic.Statement.Fields where

import Data.Aeson qualified as Aeson
import Database.Generic.Entity.FieldName (FieldName, HasFieldName, fieldName)
import Database.Generic.Prelude
import Database.Generic.Statement.Order (Order(..))
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
  fieldNames :: fs -> [FieldName] -- TODO

-- | Ordered fields 'fs' of 'a'.
class OrderedFieldsOf fs a | fs -> a where
  -- | The names of the fields to be selected.
  orderedFieldNames :: fs -> OrderedFields

-- * field - field3

-- | A named field of type 'b' belonging to 'a'.
--
-- This can be used to filter results to only a subset of 'a's fields.
newtype Field a b = Field { name :: FieldName } deriving Generic

instance FieldsOf (Field a b) a b where
  fieldNames fb = [fb.name]

field :: forall f a b. (HasField f a b, HasFieldName f) => Field a b
field = Field $ fieldName @f

newtype Field2 a b c = Field2 (Field a b, Field a c)

instance FieldsOf (Field2 a b c) a (b, c) where
  fieldNames (Field2 (fb, fc)) = [fb.name, fc.name]

field2
  :: forall fb fc a b c.
  ( HasField fb a b, HasFieldName fb
  , HasField fc a c, HasFieldName fc
  ) => Field2 a b c
field2 = Field2 (field @fb @a @b, field @fc @a @c)

newtype Field3 a b c d = Field3 (Field a b, Field a c, Field a d)

instance FieldsOf (Field3 a b c d) a (b, c, d) where
  fieldNames (Field3 (fb, fc, fd)) = [fb.name, fc.name, fd.name]

field3
  :: forall fb fc fd a b c d.
  ( HasField fb a b, HasFieldName fb
  , HasField fc a c, HasFieldName fc
  , HasField fd a d, HasFieldName fd
  ) => Field3 a b c d
field3 = Field3 (field @fb @a @b, field @fc @a @c, field @fd @a @d)

-- * fieldOrder - fieldOrder3

-- | A named field belonging to 'a', with ordering 'o'.
--
-- This can be used to order a query of a collection of 'a's.
newtype FieldOrder a (o :: Order) = FieldOrder { name :: FieldName }
  deriving Generic

instance OrderedFieldsOf (FieldOrder a Asc) a where
  orderedFieldNames fb = OrderedFields [(fb.name, Asc)]

instance OrderedFieldsOf (FieldOrder a Desc) a where
  orderedFieldNames fb = OrderedFields [(fb.name, Desc)]

fieldOrder
  :: forall f (o :: Order) a b
  . (HasField f a b, HasFieldName f)
  => FieldOrder a o
fieldOrder = FieldOrder (fieldName @f)

newtype FieldOrder2 a o1 o2 =
  FieldOrder2 (FieldOrder a o1, FieldOrder a o2)

instance
  ( OrderedFieldsOf (FieldOrder a o1) a
  , OrderedFieldsOf (FieldOrder a o2) a
  ) => OrderedFieldsOf (FieldOrder2 a o1 o2) a where
  orderedFieldNames (FieldOrder2 (fb, fc)) =
    orderedFieldNames fb <> orderedFieldNames fc

fieldOrder2 :: forall fb (o1 :: Order) fc (o2 :: Order) a b c.
  ( HasField fb a b, HasFieldName fb
  , HasField fc a c, HasFieldName fc
  ) => FieldOrder2 a o1 o2
fieldOrder2 = FieldOrder2
  ( fieldOrder @fb @o1 @a @b
  , fieldOrder @fc @o2 @a @c
  )
