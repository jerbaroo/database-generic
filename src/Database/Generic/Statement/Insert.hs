
module Database.Generic.Statement.Insert where

import Database.Generic.Entity (Entity)
import Database.Generic.Entity qualified as Entity
import Database.Generic.Entity.SqlTypes (SqlValue(..))
import Database.Generic.Entity.ToSql (HasSqlFieldNames(..), toSqlValues)
import Database.Generic.Statement.Returning (Returning(..))
import Database.Generic.Statement.Values (Values(..))
import Database.Generic.Prelude
import Database.Generic.Serialize (Serialize(..))
import Database.Generic.Table (TableName)

-- | Insert statement with return type 'r'.
data Insert (r :: Returning) = Insert
  { table   :: !TableName
  , columns :: ![String]
  , rows    :: ![Values]
  }

instance Serialize SqlValue db => Serialize (Insert r) db where
  serialize i = unwords
    [ "INSERT INTO", serialize i.table
    , "(", intercalate ", " i.columns, ") VALUES"
    , intercalate ", " $ serialize @_ @db <$> i.rows
    , ";"
    ]

insertOne :: forall a f. Entity f a => a -> Insert (OneAffected a)
insertOne a = Insert
  { table   = Entity.tableName @_ @a
  , columns = sqlFieldNames @a
  , rows    = [Values $ toSqlValues a]
  }
