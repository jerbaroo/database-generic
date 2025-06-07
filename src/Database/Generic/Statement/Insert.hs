module Database.Generic.Statement.Insert where

import Database.Generic.Entity (Entity)
import Database.Generic.Entity.EntityName (EntityName(..), entityName)
import Database.Generic.Entity.FieldName (FieldName)
import Database.Generic.Entity.FieldName qualified as Entity
import Database.Generic.Entity.SqlTypes (SqlValue(..))
import Database.Generic.Entity.ToSql (toSqlValues)
import Database.Generic.Prelude
import Database.Generic.Serialize (Serialize(..))
import Database.Generic.Statement.Fields (Fields(..), ReturnFields(..))
import Database.Generic.Statement.Fields qualified as Fields
import Database.Generic.Statement.Returning (ModifyReturning, Returnable(..), Returning)
import Database.Generic.Statement.Type.OneOrMany (OneOrMany(..))
import Database.Generic.Statement.Values (Values(..))
import Witch qualified as W

-- | Insert one or many values of type 'a', maybe returning fields 'fs'.
data Insert (o :: OneOrMany) (r :: Maybe fs) a = Insert
  { into       :: !EntityName
  , fieldNames :: ![FieldName]
  , returning  :: !(Maybe Fields)
  , values     :: ![Values]
  }

type instance Returning (Insert _ Nothing   a) = a
type instance Returning (Insert _ (Just fs) _) = fs

type instance ModifyReturning (Insert o _ a) fs = Insert o (Just fs) a

instance Returnable (Insert o Nothing a) (Insert o (Just a) a) where
  returning i = Insert
    { into       = i.into
    , fieldNames = i.fieldNames
    , returning  = Just All
    , values     = i.values
    }

instance ReturnFields (Insert o r a) where
  fields i f = Insert
    { into       = i.into
    , fieldNames = i.fieldNames
    , returning = Just $ Some $ Fields.fieldNames f
    , values     = i.values
    }

instance Serialize SqlValue db => Serialize (Insert o r a) db where
  serialize i = unwords $
    [ "INSERT INTO", W.from i.into
    , "(", intercalate ", " $ from <$> i.fieldNames, ") VALUES"
    , intercalate ", " $ serialize @_ @db <$> i.values
    ]
    <> maybe [] (\c -> ["RETURNING " <> serialize c]) i.returning
    <> [ ";" ]

insertOne :: forall a. Entity a => a -> Insert One Nothing a
insertOne a = Insert
  { into       = entityName @a
  , fieldNames = Entity.fieldNames @a
  , returning  = Nothing
  , values     = [Values $ toSqlValues a]
  }

insertMany :: forall a. Entity a => [a] -> Insert Many Nothing a
insertMany as = Insert
  { into       = entityName @a
  , fieldNames = Entity.fieldNames @a
  , returning  = Nothing
  , values     = Values . toSqlValues <$> as
  }
