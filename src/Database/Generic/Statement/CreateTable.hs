module Database.Generic.Statement.CreateTable where

import Database.Generic.Entity (Entity')
import Database.Generic.Entity.EntityName (EntityName(..), entityName)
import Database.Generic.Entity.FieldName (FieldName)
import Database.Generic.Entity.PrimaryKey (primaryKeyFieldName)
import Database.Generic.Entity.SqlColumns (HasSqlColumns(..))
import Database.Generic.Entity.SqlTypes (SqlTypeId)
import Database.Generic.Prelude
import Database.Generic.Serialize (Serialize(..))
import Database.Generic.Serialize qualified as Serialize

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
  serialize c = Serialize.statement $ unwords $ catMaybes
    [ Just "CREATE TABLE"
    , if c.ifNotExists then Just "IF NOT EXISTS" else Nothing
    , Just $ from c.name
    , Just $ Serialize.parens $ c.columns <&> \c' -> unwords $ catMaybes
          [ Just $ from c'.name
          , Just $ serialize @_ @db c'.type'
          , if c'.primary then Just "PRIMARY KEY" else Nothing
          ]
    ]

createTable :: forall a f b. Entity' a f b => Bool -> CreateTable a
createTable ifNotExists = do
  let primaryName = primaryKeyFieldName @a
  let columns = sqlColumns @a <&> \(name, type') ->
        CreateTableColumn { primary = name == primaryName, .. }
  CreateTable { columns, name = entityName @a, .. }
