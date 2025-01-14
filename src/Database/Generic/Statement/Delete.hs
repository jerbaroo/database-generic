module Database.Generic.Statement.Delete where

import Database.Generic.Entity (Entity)
import Database.Generic.Entity qualified as Entity
import Database.Generic.Entity.SqlTypes (SqlValue(..))
import Database.Generic.Entity.ToSql (ToSqlValue(..))
import Database.Generic.Statement.Where (Where, idEquals)
import Database.Generic.Prelude
import Database.Generic.Serialize (Serialize(..))
import Database.Generic.Table (TableName)

data OneOrMany = One | Many

-- | Delete one or many values of type 'a'.
data Delete (o :: OneOrMany) a = Delete
  { from   :: !TableName
  , where' :: !(Maybe (Where a))
  }

instance Serialize SqlValue db => Serialize (Delete o a) db where
  serialize d = unwords $
    ["DELETE FROM", serialize d.from]
    <> maybe [] (\w -> ["WHERE", serialize @_ @db w]) d.where'
    <> [ ";" ]

deleteAll :: forall a f. Entity f a => Delete Many a
deleteAll = Delete
  { from   = Entity.tableName @_ @a
  , where' = Nothing
  }

deleteById :: forall a f b.
  (Entity f a, HasField f a b, ToSqlValue b) => b -> Delete One a
deleteById b = Delete
  { from   = Entity.tableName @_ @a
  , where' = Just $ idEquals @a b
  }
