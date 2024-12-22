{-# OPTIONS_GHC -Wno-loopy-superclass-solve #-}
module Database.Generic.Statement where

import Database.Generic.Database (Database(..))
import Database.Generic.Entity (Entity)
import Database.Generic.Entity qualified as Entity
import Database.Generic.Entity.SqlTypes (SqlTypeId)
import Database.Generic.Prelude

newtype TableName = TableName String

instance Show TableName where
  show (TableName s) = s

tableName :: forall a f. Entity f a => TableName
tableName = TableName $ Entity.tableName @_ @a

-- * Statement

data Statement
  = StatementCreateTable CreateTable

class Database db => SerializeStatement a db where
  serializeStatement :: a -> String

instance SerializeStatement CreateTable db => (SerializeStatement Statement db) where
  serializeStatement (StatementCreateTable c) = serializeStatement @_ @db c

-- * Create Table

data CreateTable = CreateTable
  { ifNotExists :: Bool
  , name        :: TableName
  , columns     :: [CreateTableColumn]
  }

data CreateTableColumn = CreateTableColumn
  { name    :: !String
  , primary :: !Bool
  , type'   :: !SqlTypeId
  }

instance Database db => SerializeStatement CreateTable db where
  serializeStatement c = unwords
    [ "CREATE TABLE"
    , if c.ifNotExists then "IF NOT EXISTS" else ""
    , show c.name
    , "("
    , intercalate ", " $ c.columns <&> \c' -> unwords
          [ c'.name
          , columnType @db c'.type'
          , if c'.primary then "PRIMARY KEY" else ""
          ]
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
