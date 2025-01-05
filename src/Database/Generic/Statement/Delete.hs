module Database.Generic.Statement.Delete where

import Database.Generic.Entity (Entity)
import Database.Generic.Entity qualified as Entity
import Database.Generic.Entity.SqlTypes (SqlValue(..))
import Database.Generic.Entity.ToSql (ToSqlValue(..))
import Database.Generic.Statement.Returning (Returning(..))
import Database.Generic.Statement.Where (Wheres(..), idEquals)
import Database.Generic.Prelude
import Database.Generic.Serialize (Serialize(..))
import Database.Generic.Table (TableName)

-- | Delete values of type 'a'.
data Delete (r :: Returning a) = Delete
  { from   :: !TableName
  , where' :: !Wheres
  }

instance Serialize SqlValue db => Serialize (Delete r) db where
  serialize d = unwords
    [ "DELETE FROM", serialize d.from, "WHERE", serialize @_ @db d.where', ";"]

deleteById :: forall a f b.
  (Entity f a, HasField f a b, ToSqlValue b) => b -> Delete (OneAffected a)
deleteById b = Delete
  { from   = Entity.tableName @_ @a
  , where' = Wheres [idEquals @a b]
  }
