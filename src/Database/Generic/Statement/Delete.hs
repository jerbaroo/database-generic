module Database.Generic.Statement.Delete where

import Database.Generic.Entity (Entity)
import Database.Generic.Entity qualified as Entity
import Database.Generic.Entity.SqlTypes (SqlValue(..))
import Database.Generic.Entity.ToSql (ToSqlValue(..))
import Database.Generic.Prelude
import Database.Generic.Serialize (Serialize(..))
import Database.Generic.Statement.Fields (ReturningFields(..), fieldNames)
import Database.Generic.Statement.Returning (NowReturning, Returnable(..), Returning)
import Database.Generic.Statement.Type.OneOrMany (OneOrMany(..))
import Database.Generic.Statement.Where (Where, idEquals)
import Database.Generic.Table (TableName)

data Columns = All | Some ![String]

instance Serialize Columns db where
  serialize All       = "*"
  serialize (Some cs) = intercalate ", " cs

-- | Delete one or many values of type 'a', maybe returning fields 'fs'.
data Delete (o :: OneOrMany) (r :: Maybe fs) a = Delete
  { columns :: !(Maybe Columns)
  , from    :: !TableName
  , where'  :: !(Maybe (Where a))
  }

type instance Returning (Delete _ (Just fs) _) = fs

type instance NowReturning (Delete o _ a) fs = Delete o (Just fs) a


instance Serialize SqlValue db => Serialize (Delete o r a) db where
  serialize d = unwords $
    ["DELETE FROM", serialize d.from]
    <> maybe [] (\w -> ["WHERE", serialize @_ @db w]) d.where'
    <> maybe [] (\c -> ["RETURNING " <> serialize @_ @db c]) d.columns
    <> [ ";" ]

deleteAll :: forall a f. Entity f a => Delete Many Nothing a
deleteAll = Delete
  { columns = Nothing
  , from   = Entity.tableName @_ @a
  , where' = Nothing
  }

deleteById :: forall a f b.
  (Entity f a, HasField f a b, ToSqlValue b) => b -> Delete One Nothing a
deleteById b = Delete
  { columns = Nothing
  , from   = Entity.tableName @_ @a
  , where' = Just $ idEquals @a b
  }

instance Returnable (Delete o Nothing a) (Delete o (Just a) a) where
  returning d = Delete
    { columns = Just All
    , from    = d.from
    , where'  = d.where'
    }

instance ReturningFields (Delete o x a) where
  fields d p = Delete
    { columns = Just $ Some $ fieldNames p
    , from    = d.from
    , where'  = d.where'
    }
