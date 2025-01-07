module Database.Generic.Statement.Select where

import Database.Generic.Entity (Entity)
import Database.Generic.Entity qualified as Entity
import Database.Generic.Entity.SqlTypes (SqlValue(..))
import Database.Generic.Entity.ToSql (ToSqlValue)
import Database.Generic.Statement.Fields (SelectFields(..), fieldNames)
import Database.Generic.Statement.Where (Wheres(..), idEquals)
import Database.Generic.Statement.Returning (StatementType(..))
import Database.Generic.Prelude
import Database.Generic.Serialize (Serialize(..))
import Database.Generic.Table (TableName)

data Columns = ColumnsAll | Columns ![String]

instance Serialize Columns db where
  serialize ColumnsAll   = "*"
  serialize (Columns cs) = intercalate ", " cs

-- | Select statement with return type 'r'.
data Select (r :: StatementType) = Select
  { columns :: !Columns
  , from    :: !TableName
  , where'  :: !Wheres
  }

instance SelectFields Select where
  fields s p = Select
    { columns = Columns $ fieldNames p
    , from    = s.from
    , where'  = s.where'
    }

instance Serialize SqlValue db => Serialize (Select r) db where
  serialize s = unwords
    [ "SELECT", serialize s.columns
    , "FROM", serialize s.from
    , "WHERE", serialize @_ @db s.where'
    , ";"
    ]

selectById :: forall a f b.
  (Entity f a, HasField f a b, ToSqlValue b) => b -> Select (MaybeOne a)
selectById b = Select
  { columns = ColumnsAll
  , from    = Entity.tableName @_ @a
  , where'  = Wheres [idEquals @a b]
  }
