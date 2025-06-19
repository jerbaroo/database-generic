{-# LANGUAGE UndecidableInstances #-}

module Database.Generic.Statement.Insert where

import Data.Aeson qualified as Aeson
import Database.Generic.Entity.DbColumns qualified as Db
import Database.Generic.Entity.DbColumns (HasDbColumns)
import Database.Generic.Entity.DbTypes (DbValueN)
import Database.Generic.Entity.EntityName (EntityName(..), entityName, HasEntityName)
import Database.Generic.Entity.FieldName (FieldName)
import Database.Generic.Entity.ToDb (toDbValues, ToDbValues)
import Database.Generic.Prelude
import Database.Generic.Serialize (Serialize(..))
import Database.Generic.Statement.Fields (Fields(..))
import Database.Generic.Statement.Fields qualified as Fields
import Database.Generic.Statement.Returning (IsReturning, ModifyReturnType, Returning(..), ReturningFields(..), Row)
import Database.Generic.Statement.Type.OneOrMany (OneOrMany(..))
import Database.Generic.Statement.Values (Values(..))

-- | Insert one or many values of type 'a', maybe returning fields 'fs'.
newtype Insert (o :: OneOrMany) (r :: Maybe fs) a = Insert Insert'
  deriving (Eq, From Insert', Show)

instance From (Insert o r a) Insert'

data Insert' = Insert'
  { into       :: !EntityName
  , fieldNames :: ![FieldName]
  , returning  :: !(Maybe Fields)
  , values     :: ![Values]
  } deriving (Eq, Generic, Show)

instance Aeson.FromJSON Insert'

type instance ModifyReturnType (Insert o _ a) r = Insert o (Just r) a

type instance Row (Insert _ _ a) = a

instance IsReturning (Insert o (Just fs) a)

instance Returning (Insert o Nothing a) (Insert o (Just a) a) where
  returning (Insert Insert' {..}) = Insert Insert' { returning = Just All, .. }

instance ReturningFields (Insert o r a) where
  returningFields (Insert Insert' {..}) f = Insert Insert'
    { returning = Just $ Some $ Fields.fieldNames f, .. }

instance Serialize DbValueN db => Serialize Insert' db where
  serialize i = unwords $
    [ "INSERT INTO", from i.into
    , "(", intercalate ", " $ from <$> i.fieldNames, ") VALUES"
    , intercalate ", " $ serialize @_ @db <$> i.values
    ]
    <> maybe [] (\c -> ["RETURNING " <> serialize c]) i.returning
    <> [ ";" ]

insertOne :: forall a
  . (HasDbColumns a, HasEntityName a, ToDbValues a)
  => a -> Insert One Nothing a
insertOne a = Insert Insert'
  { into       = entityName @a
  , fieldNames = Db.fieldNames @a
  , returning  = Nothing
  , values     = [Values $ toDbValues a]
  }

insertMany :: forall a
  . (HasDbColumns a, HasEntityName a, ToDbValues a)
  => [a] -> Insert Many Nothing a
insertMany as = Insert Insert'
  { into       = entityName @a
  , fieldNames = Db.fieldNames @a
  , returning  = Nothing
  , values     = Values . toDbValues <$> as
  }
