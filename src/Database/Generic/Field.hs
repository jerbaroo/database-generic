{-# LANGUAGE UndecidableInstances #-}

module Database.Generic.Field where

import Database.Generic.Prelude

-- | Value-level representation of a field of a data type.
data Field f a b where
  Field :: (HasField f a b, Typeable f) => Proxy f -> (a -> b) -> Field f a b

-- | Construct a 'Field'.
field :: forall f a b. (HasField f a b, Typeable f) => Field f a b
field = Field (Proxy @f) (getField @f)

-- | Types that have a single named field.
class HasFieldName f where
  fieldName :: f -> String

instance HasFieldName (Field f a b) where
  fieldName (Field _ _) = showType' @f
