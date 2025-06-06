module Database.Generic.Statement.Select where

import Database.Generic.Entity (Entity, EntityP)
import Database.Generic.Entity qualified as Entity
import Database.Generic.Entity.EntityName (EntityName)
import Database.Generic.Entity.SqlTypes (SqlValue(..))
import Database.Generic.Statement.Fields (Fields(..), ReturningFields(..), fieldNames)
import Database.Generic.Statement.Type.OneOrMany (OneOrMany(..))
import Database.Generic.Statement.Where (Where(..), Whereable(..), idEquals)
import Database.Generic.Statement.Returning (NowReturning, Returning)
import Database.Generic.Prelude
import Database.Generic.Serialize (Serialize(..))
import Witch qualified as W

-- | Select one or many values of type 'a', but only fields 'fs'.
data Select (o :: OneOrMany) fs a = Select
  { entityName :: !EntityName
  , fields     :: !Fields
  , where'     :: !(Maybe (Where a))
  }

type instance Returning (Select _ fs _) = fs

type instance NowReturning (Select o a a) fs = Select o fs a

instance ReturningFields (Select o a a) where
  fields s p = Select
    { entityName = s.entityName
    , fields     = Some $ fieldNames p
    , where'     = s.where'
    }

instance Serialize SqlValue db => Serialize (Select o fs a) db where
  serialize s = unwords $
    ["SELECT", serialize s.fields, "FROM", W.from s.entityName ]
    <> maybe [] (\w -> ["WHERE", serialize @_ @db w]) s.where'
    <> [ ";" ]

instance Whereable (Select o fs a) a where
  where' s w = s { where' = s.where' <&> (`And` w) }

selectAll :: forall a. Entity a => Select Many a a
selectAll = Select
  { entityName = Entity.entityName @a
  , fields     = All
  , where'     = Nothing
  }

selectById :: forall a f b. EntityP a f b => b -> Select One a a
selectById b = Select
  { entityName = Entity.entityName @a
  , fields     = All
  , where'     = Just $ idEquals @a b
  }
