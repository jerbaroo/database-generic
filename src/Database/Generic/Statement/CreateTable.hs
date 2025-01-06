module Database.Generic.Statement.CreateTable where

import Database.Generic.Entity (Entity)
import Database.Generic.Entity qualified as Entity
import Database.Generic.Entity.SqlTypes (SqlTypeId)
import Database.Generic.Entity.ToSql (HasSqlColumnNames(..), HasSqlColumnTypes(..))
import Database.Generic.Prelude
import Database.Generic.Serialize (Serialize(..))
import Database.Generic.Table (ColumnName(..), TableName)

-- | Create a table for values of type 'a'.
data CreateTable a = CreateTable
  { ifNotExists :: !Bool
  , name        :: !TableName
  , columns     :: ![CreateTableColumn]
  }

data CreateTableColumn = CreateTableColumn
  { name    :: !ColumnName
  , primary :: !Bool
  , type'   :: !SqlTypeId
  }

instance Serialize SqlTypeId db => Serialize (CreateTable a) db where
  serialize c = unwords
    [ "CREATE TABLE"
    , if c.ifNotExists then "IF NOT EXISTS" else ""
    , serialize @_ @db c.name
    , "("
    , intercalate ", " $ c.columns <&> \c' -> unwords
          [ from c'.name
          , serialize @_ @db c'.type'
          , if c'.primary then "PRIMARY KEY" else ""
          ]
    , ");"
    ]

createTable :: forall a f. Entity f a => Bool -> CreateTable a
createTable ifNotExists = do
  let primaryName = Entity.primaryKeyFieldName @a
  let columns =
        zip (sqlColumnNames @a) (sqlFieldTypes @a) <&>
          \(name, type') -> CreateTableColumn
            { primary = from name == primaryName, .. }
  CreateTable { name = Entity.tableName @_ @a, columns, .. }
