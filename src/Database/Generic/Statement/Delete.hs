module Database.Generic.Statement.Delete where

import Database.Generic.Entity (Entity, EntityP)
import Database.Generic.Entity.EntityName (EntityName, entityName)
import Database.Generic.Entity.SqlTypes (SqlValue(..))
import Database.Generic.Prelude
import Database.Generic.Serialize (Serialize(..))
import Database.Generic.Statement.Fields (Fields(..), ReturnFields(..), fieldNames)
import Database.Generic.Statement.Returning (IsReturning, ModifyReturnType, Returning(..), Row)
import Database.Generic.Statement.Type.OneOrMany (OneOrMany(..))
import Database.Generic.Statement.Where (Where, idEquals)
import Witch qualified as W

-- | Delete one or many values of type 'a', maybe returning fields 'fs'.
data Delete (o :: OneOrMany) (r :: Maybe fs) a = Delete
  { from      :: !EntityName
  , returning :: !(Maybe Fields)
  , where'    :: !(Maybe (Where a))
  }

-- | 'fs' represents the type of Haskell values returned.
type instance IsReturning (Delete _ (Just fs) _) = fs
-- type instance Returning (Delete _ Nothing   _) = Nothing

-- | Modify the type of a delete statement to reflect a subset of fields returned.
--
-- Note the 'Nothing', which means the statement return type may only be
-- modified if it has not already been modified to return something. Will
-- consider relaxing this constraint once the library is stable.
type instance ModifyReturnType (Delete o Nothing a) fs = Delete o (Just fs) a

type instance Row (Delete _ _ a) = a

-- | An insert statement not returning anything can be modified to return 'a's.
instance Returning (Delete o Nothing a) (Delete o (Just a) a) where
  returning d = Delete
    { from      = d.from
    , returning = Just All
    , where'    = d.where'
    }

instance ReturnFields (Delete o Nothing a) where
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
