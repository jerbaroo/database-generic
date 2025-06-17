{-# LANGUAGE UndecidableInstances #-}

module Database.Generic.Statement.Select where

import Data.Aeson qualified as Aeson
import Database.Generic.Entity.DbTypes (DbValueN)
import Database.Generic.Entity.EntityName (EntityName, HasEntityName)
import Database.Generic.Entity.EntityName qualified as Entity
import Database.Generic.Entity.PrimaryKey as X (PrimaryKey')
import Database.Generic.Statement.Fields (Fields(..), OrderedFields(..), fieldNames, orderedFieldNames)
import Database.Generic.Statement.Limit (Limit, Limitable(..), Offset)
import Database.Generic.Statement.OrderBy (IsOrderedBy, OrderBy(..), ModifyOrderedBy)
import Database.Generic.Statement.Type.OneOrMany (OneOrMany(..))
import Database.Generic.Statement.Where (Where(..), Whereable(..), idEquals)
import Database.Generic.Statement.Returning (IsReturning, ModifyReturnType, ReturningFields(..), Row)
import Database.Generic.Prelude
import Database.Generic.Serialize (Serialize(..))
import Database.Generic.Serialize qualified as Serialize
import Database.Generic.Entity.ToDb (ToDbValue)
import Witch qualified as W

-- | Select one or many values from a collection of 'a'.
newtype Select (o :: OneOrMany) fs a (ob :: Bool) = Select Select'
  deriving (Eq, From Select', Show)

instance From (Select o fs a ob) Select'

data Select' = Select'
  { fields  :: !Fields
  , from    :: !EntityName
  , limit   :: !(Maybe Limit)
  , offset  :: !(Maybe Offset)
  , orderBy :: !OrderedFields
  , where'  :: !(Maybe Where)
  } deriving (Eq, Generic, Show)

instance Aeson.FromJSON Select'

type instance ModifyOrderedBy (Select o fs a _) = Select o fs a True

type instance ModifyReturnType (Select o _ a ob) fs = Select o fs a ob

type instance Row (Select _ _ a _) = a

instance IsOrderedBy (Select o fs a True)

instance IsReturning (Select o fs a ob)

instance Limitable (Select Many fs a True) where
  limitOffsetMay l offset (Select s) = Select s { limit = Just l, offset }

instance OrderBy (Select o fs a ob) where
  orderBy fs (Select s) = Select s { orderBy = orderedFieldNames fs }

instance ReturningFields (Select o a a ob) where
  returningFields (Select Select' {..}) fs = Select Select'
    { fields = Some $ fieldNames fs, .. }

instance (Serialize DbValueN db, Serialize OrderedFields db) => Serialize Select' db where
  serialize s = Serialize.statement $ unwords $ catMaybes
    [ Just "SELECT"
    , Just $ serialize s.fields
    , Just "FROM"
    , Just $ W.from s.from
    , s.where' <&> \w -> "WHERE " <> serialize @_ @db w
    , case s.orderBy of
        (OrderedFields []) -> Nothing
        fields             -> Just $ "ORDER BY " <> serialize @_ @db fields
    , s.limit <&> \l -> "LIMIT " <> show l
    , s.offset <&> \o -> "OFFSET " <> show o
    ]

instance Whereable (Select o fs a ob) a where
  where' (Select s) w = Select s { where' = s.where' <&> (`And` w) }

selectAll :: forall a. HasEntityName a => Select Many a a False
selectAll = Select Select'
  { fields  = All
  , from    = Entity.entityName @a
  , limit   = Nothing
  , offset  = Nothing
  , orderBy = OrderedFields []
  , where'  = Nothing
  }

selectById :: forall a f b. (HasEntityName a, PrimaryKey' a f b, ToDbValue b)
  => b -> Select One a a False
selectById b = Select Select'
  { fields  = All
  , from    = Entity.entityName @a
  , limit   = Nothing
  , offset  = Nothing
  , orderBy = OrderedFields []
  , where'  = Just $ idEquals @a b
  }
