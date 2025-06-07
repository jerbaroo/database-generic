module Database.Generic.Entity.PrimaryKey where

import Database.Generic.Entity.FieldName (FieldName, HasFieldName, fieldName)
import Database.Generic.Prelude

class HasFieldName f => PrimaryKey f a | a -> f where
  primaryKey :: forall b. HasField f a b => a -> b
  primaryKey = getField @f

-- | 'PrimaryKey' with type 'b' of primary key in scope.
type PrimaryKey' a f b = (HasField f a b, PrimaryKey f a)

primaryKeyFieldName :: forall a f. PrimaryKey f a => FieldName
primaryKeyFieldName = fieldName @f
