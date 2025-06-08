module Database.Generic.Statement.Select where

import Database.Generic.Entity (Entity, Entity')
import Database.Generic.Entity.EntityName (EntityName)
import Database.Generic.Entity.EntityName qualified as Entity
import Database.Generic.Entity.FieldName (FieldName)
import Database.Generic.Entity.SqlTypes (SqlValue(..))
import Database.Generic.Statement.Fields (Fields(..), fieldNames)
import Database.Generic.Statement.Limit (Limit, Limitable(..))
import Database.Generic.Statement.OrderBy (OrderBy(..))
import Database.Generic.Statement.Type.OneOrMany (OneOrMany(..))
import Database.Generic.Statement.Where (Where(..), Whereable(..), idEquals)
import Database.Generic.Statement.Returning (IsReturning, ModifyReturnType, ReturningFields(..), Row)
import Database.Generic.Prelude
import Database.Generic.Serialize (Serialize(..))
import Database.Generic.Serialize qualified as Serialize
import Witch qualified as W

-- | Select from one or many values of type 'a', the fields 'fs'.
data Select (o :: OneOrMany) fs a = Select
  { fields  :: !Fields
  , from    :: !EntityName
  , limit   :: !(Maybe Limit)
  , orderBy :: ![FieldName]
  , where'  :: !(Maybe (Where a))
  } deriving (Eq, Show)

type instance ModifyReturnType (Select o _ a) r = Select o r a

type instance Row (Select _ _ a) = a

instance IsReturning (Select o fs a)

instance Limitable (Select Many fs a) where
  limit l s = s { limit = Just l }

instance OrderBy (Select o fs a) where
  orderBy fs s = s { orderBy = fieldNames fs }

instance ReturningFields (Select o a a) where
  returningFields Select{..} fs = Select { fields = Some $ fieldNames fs, .. }

instance Serialize SqlValue db => Serialize (Select o fs a) db where
  serialize s = Serialize.statement $ unwords $ catMaybes
    [ Just "SELECT"
    , Just $ serialize s.fields
    , Just "FROM"
    , Just $ W.from s.from
    , s.where' <&> \w -> "WHERE " <> serialize @_ @db w
    , case s.orderBy of
        []     -> Nothing
        fields -> Just $ "ORDER BY " <> serialize (Some fields)
    , s.limit <&> \l -> "LIMIT " <> show l
    ]

instance Whereable (Select o fs a) a where
  where' s w = s { where' = s.where' <&> (`And` w) }

selectAll :: forall a f. Entity a f => Select Many a a
selectAll = Select
  { fields  = All
  , from    = Entity.entityName @a
  , limit   = Nothing
  , orderBy = []
  , where'  = Nothing
  }

selectById :: forall a f b. Entity' a f b => b -> Select One a a
selectById b = Select
  { fields  = All
  , from    = Entity.entityName @a
  , limit   = Nothing
  , orderBy = []
  , where'  = Just $ idEquals @a b
  }
