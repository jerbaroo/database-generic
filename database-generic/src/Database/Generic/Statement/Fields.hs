{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE MonoLocalBinds   #-}
{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Database.Generic.Statement.Fields where

import Data.Aeson qualified as Aeson
import Database.Generic.Entity.FieldName (FieldName, HasFieldName, fieldName)
import Database.Generic.Prelude
import Database.Generic.Serialize (Serialize(..))
import Witch qualified as W
import GHC.OverloadedLabels (IsLabel(..))
import Database.Generic.Entity.PrimaryKey (PrimaryKey)
import GHC.TypeLits (KnownSymbol, Symbol)

-- | Named fields in a statement.
data Fields = All | Some ![FieldName]
  deriving (Eq, Generic, Show)

instance Aeson.FromJSON Fields

-- TODO this is very SQL specific
instance Serialize Fields db where
  serialize All       = "*"
  serialize (Some cs) = intercalate ", " $ W.from <$> cs

-- | Fields 'fs' of 'a' that can be parsed into a 'b'.
class FieldsOf fs a b | fs -> a, fs -> b where
  -- | The names of the fields to be selected.
  fieldNames :: fs -> [FieldName]

-- * field - field3

-- | Value-level representation of a field of type 'b' belonging to 'a'.
newtype Field a b = Field { name :: FieldName } deriving Generic

instance FieldsOf (Field a b) a b where
  fieldNames fb = [fb.name]

field :: forall f a b. (HasField f a b, HasFieldName f) => Field a b
field = Field $ fieldName @f

newtype F2 a b c = F2 (Field a b, Field a c)

instance FieldsOf (F2 a b c) a (b, c) where
  fieldNames (F2 (fb, fc)) = [fb.name, fc.name]

field2 :: forall fb fc a b c.
  ( HasField fb a b, HasFieldName fb
  , HasField fc a c, HasFieldName fc
  ) => F2 a b c
field2 = F2 (field @fb @a @b, field @fc @a @c)

newtype F3 a b c d = F3 (Field a b, Field a c, Field a d)

instance FieldsOf (F3 a b c d) a (b, c, d) where
  fieldNames (F3 (fb, fc, fd)) = [fb.name, fc.name, fd.name]

field3 :: forall fb fc fd a b c d.
  ( HasField fb a b, HasFieldName fb
  , HasField fc a c, HasFieldName fc
  , HasField fd a d, HasFieldName fd
  ) => F3 a b c d
field3 = F3 (field @fb @a @b, field @fc @a @c, field @fd @a @d)

-- * fieldOrder - fieldOrder3

data Order = Asc | Desc

data Label (f :: Symbol) = L

data Field' f a b = Field'

instance IsLabel f (Field' f a b) where
  fromLabel = Field'

data Person = Person { age :: !Int64, name :: !String }
  deriving (Generic, PrimaryKey "name", Show)

x :: Field' "name" a b
x = Field' @"name"

x2 :: Field' "name" a b
x2 = #name

data F2' a f1 f2 b1 b2 = F2' (Field' f1 a b1, Field' f2 a b2)

field2' :: forall a f1 f2 b1 b2
  .  (HasField f1 a b1, HasField f2 a b2)
  => Field' f1 a b1 -> Field' f2 a b2 -> F2' a f1 f2 b1 b2
field2' f1 f2 = F2' (f1, f2)

x3 :: F2' Person "name" "age" _ _
x3 = field2' @Person #name #age
