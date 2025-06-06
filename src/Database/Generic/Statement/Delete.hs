module Database.Generic.Statement.Delete where

import Database.Generic.Entity (Entity, EntityP)
import Database.Generic.Entity.EntityName (EntityName, entityName)
import Database.Generic.Entity.SqlTypes (SqlValue(..))
import Database.Generic.Prelude
import Database.Generic.Serialize (Serialize(..))
import Database.Generic.Statement.Fields (Fields(..), ReturningFields(..), fieldNames)
import Database.Generic.Statement.Returning (NowReturning, Returnable(..), Returning)
import Database.Generic.Statement.Type.OneOrMany (OneOrMany(..))
import Database.Generic.Statement.Where (Where, idEquals)
import Witch qualified as W

-- | Delete one or many values of type 'a', maybe returning fields 'fs'.
data Delete (o :: OneOrMany) (r :: Maybe fs) a = Delete
  { from      :: !EntityName
  , returning :: !(Maybe Fields)
  , where'    :: !(Maybe (Where a))
  }

type instance Returning (Delete _ Nothing   a) = a
type instance Returning (Delete _ (Just fs) _) = fs

type instance NowReturning (Delete o _ a) fs = Delete o (Just fs) a

instance Returnable (Delete o Nothing a) (Delete o (Just a) a) where
  returning d = Delete
    { from      = d.from
    , returning = Just All
    , where'    = d.where'
    }

instance ReturningFields (Delete o r a) where
  fields d f = Delete
    { from      = d.from
    , returning = Just $ Some $ fieldNames f
    , where'    = d.where'
    }

instance Serialize SqlValue db => Serialize (Delete o r a) db where
  serialize d = unwords $
    ["DELETE FROM", W.from d.from]
    <> maybe [] (\w -> ["WHERE", serialize @_ @db w]) d.where'
    <> maybe [] (\c -> ["RETURNING " <> serialize c]) d.returning
    <> [ ";" ]

deleteAll :: forall a. Entity a => Delete Many Nothing a
deleteAll = Delete
  { from      = entityName @a
  , returning = Nothing
  , where'    = Nothing
  }

deleteById :: forall a f b. EntityP a f b => b -> Delete One Nothing a
deleteById b = Delete
  { from      = entityName @a
  , returning = Nothing
  , where'    = Just $ idEquals @a b
  }
