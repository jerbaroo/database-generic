-- TODO rename to Fields
module Database.Generic.Statement.Projection where

import Database.Generic.Field (Field, fieldType)
import Database.Generic.Statement.Returning (Returning(..), ReturningType)
import Database.Generic.Prelude

-- TODO class ToFields
class Projection p a b | p -> a, p -> b where
  fieldTypes :: p -> [String] -- TODO rename to fieldNames

instance Projection (Field fa a b) a b where
  fieldTypes fb = [fieldType fb]

instance Projection (Field fa a b, Field fc a c) a (b, c) where
  fieldTypes (fb, fc) = [fieldType fb, fieldType fc]

-- TODO class ReturningFields
type ReturningProjected :: forall r a b. r a -> b -> r b
type family ReturningProjected r b where
  ReturningProjected (MaybeOne _) b = MaybeOne b

-- TODO class Fields
class Projectible s where
  project :: forall p b r. (Projection p (ReturningType r) b) =>
    s r -> p -> s (ReturningProjected r b)
