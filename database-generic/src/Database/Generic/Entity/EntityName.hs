module Database.Generic.Entity.EntityName where

import Data.Aeson qualified as Aeson
import Database.Generic.Prelude

-- | Identifier for a collection in a database (e.g. "Person" table).
newtype EntityName = EntityName String
  deriving (Eq, Generic, Show)

instance Aeson.FromJSON EntityName

instance From EntityName String

instance IsString EntityName where
  fromString = EntityName

class HasEntityName a where
  entityName :: EntityName

instance Typeable a => HasEntityName a where
  entityName = EntityName $ showType' @a
