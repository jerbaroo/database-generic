module Database.Generic.Statement.Select where

import Database.Generic.Entity (Entity, Entity')
import Database.Generic.Entity.EntityName (EntityName)
import Database.Generic.Entity.EntityName qualified as Entity
import Database.Generic.Entity.SqlTypes (SqlValue(..))
import Database.Generic.Statement.Fields (Fields(..), fieldNames)
import Database.Generic.Statement.Limit (Limit, Limitable(..))
import Database.Generic.Statement.Type.OneOrMany (OneOrMany(..))
import Database.Generic.Statement.Where (Where(..), Whereable(..), idEquals)
import Database.Generic.Statement.Returning (IsReturning, ModifyReturnType, ReturningFields(..), Row)
import Database.Generic.Prelude
import Database.Generic.Serialize (Serialize(..))
import Database.Generic.Serialize qualified as Serialize
import Witch qualified as W

-- | Select from one or many values of type 'a', the fields 'fs'.
data Select (o :: OneOrMany) fs a = Select
  { entityName :: !EntityName
  , fields     :: !Fields
  , limit      :: !(Maybe Limit)
  , where'     :: !(Maybe (Where a))
  }

type instance IsReturning (Select _ fs _) = fs

type instance ModifyReturnType (Select o _ a) r = Select o r a

type instance Row (Select _ _ a) = a

instance Limitable (Select Many fs a) where
  limit l s = s { limit = Just l }

instance ReturningFields (Select o a a) where
  returningFields Select{..} p = Select
    { fields = Some $ fieldNames p, .. }

instance Serialize SqlValue db => Serialize (Select o fs a) db where
  serialize s = Serialize.statement $ unwords $ catMaybes
    [ Just "SELECT"
    , Just $ serialize s.fields
    , Just "FROM"
    , Just $ W.from s.entityName
    , s.where' <&> \w -> "WHERE " <> serialize @_ @db w
    , s.limit <&> \l -> "LIMIT " <> show l
    ]

instance Whereable (Select o fs a) a where
  where' s w = s { where' = s.where' <&> (`And` w) }

selectAll :: forall a f. Entity a f => Select Many a a
selectAll = Select
  { entityName = Entity.entityName @a
  , fields     = All
  , limit      = Nothing
  , where'     = Nothing
  }

selectById :: forall a f b. Entity' a f b => b -> Select One a a
selectById b = Select
  { entityName = Entity.entityName @a
  , fields     = All
  , limit      = Nothing
  , where'     = Just $ idEquals @a b
  }
