module Database.Generic.Statement where

import Database.Generic.Database (Database(..))
import Database.Generic.Entity (Entity)
import Database.Generic.Entity qualified as Entity
import Database.Generic.Entity.SqlTypes (SqlType)
import Database.Generic.Prelude

class Database db => ToStatement a db where
  toStatement :: a -> String

newtype TableName = TableName String deriving Show

tableName :: forall a f. Entity f a => TableName
tableName = TableName $ Entity.tableName @_ @a

-- * Create table.

data CreateTable = CreateTable
  { ifNotExists :: Bool
  , name        :: TableName
  , columns     :: [CreateTableColumn]
  }

data CreateTableColumn = CreateTableColumn
  { name    :: !String
  , primary :: !Bool
  , type'   :: !SqlType
  }

instance Database db => ToStatement CreateTable db where
  toStatement c = intercalate " "
    [ "CREATE TABLE"
    , if c.ifNotExists then "IF NOT EXISTS" else ""
    , show c.name
    , intercalate "," $ c.columns <&> \c' -> intercalate " "
          [ c'.name
          , columnType @db c'.type'
          , if c'.primary then "PRIMARY KEY" else ""
          ]
    , "("
    , ");"
    ]

createTable :: forall a f. Entity f a => Bool -> CreateTable
createTable ifNotExists = do
  let primaryName = Entity.primaryKeyFieldName @_ @a
  let columns =
        zip (Entity.sqlFieldNames @_ @a) (Entity.sqlFieldTypes @_ @a) <&>
          \(name, type') -> CreateTableColumn
            { primary = name == primaryName, .. }
  CreateTable { name = tableName @a, columns, .. }
