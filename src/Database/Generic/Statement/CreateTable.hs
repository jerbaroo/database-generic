module Database.Generic.Statement.CreateTable where

import Database.Generic.Entity (EntityP)
import Database.Generic.Entity qualified as Entity
import Database.Generic.Entity.EntityName (EntityName(..))
import Database.Generic.Entity.FieldName (FieldName)
import Database.Generic.Entity.PrimaryKey (primaryKeyFieldName)
import Database.Generic.Entity.SqlColumns (HasSqlColumns(..))
import Database.Generic.Entity.SqlTypes (SqlTypeId)
import Database.Generic.Prelude
import Database.Generic.Serialize (Serialize(..))

-- | Create a table for values of type 'a'.
data CreateTable a = CreateTable
  { columns     :: ![CreateTableColumn]
  , ifNotExists :: !Bool
  , name        :: !EntityName
  } deriving (Eq, Show)

data CreateTableColumn = CreateTableColumn
  { name    :: !FieldName
  , primary :: !Bool
  , type'   :: !SqlTypeId
  } deriving (Eq, Show)

instance Serialize SqlTypeId db => Serialize (CreateTable a) db where
  serialize c = unwords
    [ "CREATE TABLE"
    , if c.ifNotExists then "IF NOT EXISTS" else ""
    , from c.name
    , "("
    , intercalate ", " $ c.columns <&> \c' -> unwords
          [ from c'.name
          , serialize @_ @db c'.type'
          , if c'.primary then "PRIMARY KEY" else ""
          ]
    , ");"
    ]

createTable :: forall a f b. EntityP a f b => Bool -> CreateTable a
createTable ifNotExists = do
  let primaryName = primaryKeyFieldName @a
  let columns = sqlColumns @a <&> \(name, type') ->
        CreateTableColumn { primary = name == primaryName, .. }
  CreateTable { columns, name = Entity.entityName @a, .. }
