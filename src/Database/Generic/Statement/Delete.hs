module Database.Generic.Statement.Delete where

import Database.Generic.Entity (Entity, Entity')
import Database.Generic.Entity.EntityName (EntityName, entityName)
import Database.Generic.Entity.SqlTypes (SqlValue(..))
import Database.Generic.Prelude
import Database.Generic.Serialize (Serialize(..))
import Database.Generic.Statement.Fields (Fields(..), fieldNames)
import Database.Generic.Statement.Returning (IsReturning, ModifyReturnType, Returning(..), ReturningFields(..), Row)
import Database.Generic.Statement.Type.OneOrMany (OneOrMany(..))
import Database.Generic.Statement.Where (Where, idEquals)
import Witch qualified as W

-- | Delete one or many values of type 'a', maybe returning fields 'fs'.
data Delete (o :: OneOrMany) (r :: Maybe fs) a = Delete
  { fields :: !(Maybe Fields)
  , from   :: !EntityName
  , where' :: !(Maybe (Where a))
  } deriving (Eq, Show)

type instance ModifyReturnType (Delete o _ a) r = Delete o (Just r) a

type instance Row (Delete _ _ a) = a

instance IsReturning (Delete o (Just fs) a)

instance Returning (Delete o Nothing a) (Delete o (Just a) a) where
  returning Delete{..} = Delete { fields = Just All, .. }

instance ReturningFields (Delete o Nothing a) where
  returningFields Delete{..} f = Delete
    { fields = Just $ Some $ fieldNames f, .. }

instance Serialize SqlValue db => Serialize (Delete o r a) db where
  serialize d = unwords $
    ["DELETE FROM", W.from d.from]
    <> maybe [] (\w -> ["WHERE", serialize @_ @db w]) d.where'
    <> maybe [] (\c -> ["RETURNING " <> serialize c]) d.fields
    <> [ ";" ]

deleteAll :: forall a f. Entity a f => Delete Many Nothing a
deleteAll = Delete
  { fields = Nothing
  , from   = entityName @a
  , where' = Nothing
  }

deleteById :: forall a f b. Entity' a f b => b -> Delete One Nothing a
deleteById b = Delete
  { fields = Nothing
  , from   = entityName @a
  , where' = Just $ idEquals @a b
  }
