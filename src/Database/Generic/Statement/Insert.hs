
module Database.Generic.Statement.Insert where

import Database.Generic.Entity (Entity)
import Database.Generic.Entity qualified as Entity
import Database.Generic.Entity.SqlTypes (SqlValue(..))
import Database.Generic.Entity.ToSql (HasSqlColumnNames(..), toSqlValues)
import Database.Generic.Statement.Returning (StatementType(..))
import Database.Generic.Statement.Values (Values(..))
import Database.Generic.Prelude
import Database.Generic.Serialize (Serialize(..))
import Database.Generic.Table (ColumnName, TableName)

-- | Insert statement with return type 'r'.
data Insert (r :: StatementType) = Insert
  { table   :: !TableName
  , columns :: ![ColumnName]
  , rows    :: ![Values]
  }

instance Serialize SqlValue db => Serialize (Insert r) db where
  serialize i = unwords
    [ "INSERT INTO", serialize i.table
    , "(", intercalate ", " $ from <$> i.columns, ") VALUES"
    , intercalate ", " $ serialize @_ @db <$> i.rows
    , ";"
    ]

insertOne :: forall a f. Entity f a => a -> Insert (OneAffected a)
insertOne a = Insert
  { table   = Entity.tableName @_ @a
  , columns = sqlColumnNames @a
  , rows    = [Values $ toSqlValues a]
  }

insertMany :: forall a f. Entity f a => [a] -> Insert (ManyAffected a)
insertMany as = Insert
  { table   = Entity.tableName @_ @a
  , columns = sqlColumnNames @a
  , rows    = Values . toSqlValues <$> as
  }
