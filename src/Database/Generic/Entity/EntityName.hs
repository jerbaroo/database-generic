module Database.Generic.Entity.EntityName where

import Database.Generic.Prelude

newtype EntityName = EntityName String deriving (Eq, Show)

instance From EntityName String

instance IsString EntityName where
  fromString = EntityName

-- | Identifier for a collection in a database (i.e. SQL table name)
class HasEntityName a where
  entityName :: EntityName

instance Typeable a => HasEntityName a where
  entityName = EntityName $ toLower <$> showType @a
