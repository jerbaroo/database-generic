module Database.Generic.Statement.Delete where

import Database.Generic.Entity (Entity)
import Database.Generic.Entity qualified as Entity
import Database.Generic.Entity.SqlTypes (SqlValue(..))
import Database.Generic.Entity.ToSql (ToSqlValue(..))
import Database.Generic.Statement.Where (Where, idEquals)
import Database.Generic.Prelude
import Database.Generic.Serialize (Serialize(..))
import Database.Generic.Table (TableName)
import Witch qualified as W

data OneOrMany = One | Many

-- | Delete one or many values of type 'a'.
data Delete (o :: OneOrMany) a = Delete
  { from   :: !TableName
  , where' :: !(Where a)
  }

instance Serialize SqlValue db => Serialize (Delete o a) db where
  serialize d = unwords
    [ "DELETE FROM", serialize d.from, "WHERE", serialize @_ @db d.where', ";"]

deleteById :: forall a f b.
  (Entity f a, HasField f a b, ToSqlValue b) => b -> Delete One a
deleteById b = Delete
  { from   = Entity.tableName @_ @a
  , where' = W.from $ idEquals @a b
  }
