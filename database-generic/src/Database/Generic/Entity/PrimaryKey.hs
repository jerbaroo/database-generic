module Database.Generic.Entity.PrimaryKey where

import Database.Generic.Entity.FieldName (FieldName, HasFieldName, fieldName)
import Database.Generic.Prelude

-- | Select a field of a data type as the primary key.
class HasFieldName f => PrimaryKey f a | a -> f where

-- | 'PrimaryKey' with type 'b' of primary key in scope.
type PrimaryKey' a f b = (HasField f a b, PrimaryKey f a)

primaryKey :: forall a f b. PrimaryKey' a f b => a -> b
primaryKey = getField @f

primaryKeyFieldName :: forall a f. PrimaryKey f a => FieldName
primaryKeyFieldName = fieldName @f
