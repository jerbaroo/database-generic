
module Database.Generic.Statement.Insert where

import Database.Generic.Entity (Entity)
import Database.Generic.Entity qualified as Entity
import Database.Generic.Entity.SqlTypes (SqlValue(..))
import Database.Generic.Entity.ToSql (HasSqlColumnNames(..), toSqlValues)
import Database.Generic.Prelude
import Database.Generic.Serialize (Serialize(..))
import Database.Generic.Statement.Values (Values(..))
import Database.Generic.Table (ColumnName, TableName)

data OneOrMany = One | Many

-- | Insert one or many values of type 'a'.
data Insert (o :: OneOrMany) a = Insert
  { table   :: !TableName
  , columns :: ![ColumnName]
  , rows    :: ![Values]
  }

instance Serialize SqlValue db => Serialize (Insert o a) db where
  serialize i = unwords
    [ "INSERT INTO", serialize i.table
    , "(", intercalate ", " $ from <$> i.columns, ") VALUES"
    , intercalate ", " $ serialize @_ @db <$> i.rows
    , ";"
    ]

insertOne :: forall a f. Entity f a => a -> Insert One a
insertOne a = Insert
  { table   = Entity.tableName @_ @a
  , columns = sqlColumnNames @a
  , rows    = [Values $ toSqlValues a]
  }

insertMany :: forall a f. Entity f a => [a] -> Insert Many a
insertMany as = Insert
  { table   = Entity.tableName @_ @a
  , columns = sqlColumnNames @a
  , rows    = Values . toSqlValues <$> as
  }
