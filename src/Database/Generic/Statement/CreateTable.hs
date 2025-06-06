module Database.Generic.Statement.CreateTable where

import Database.Generic.Entity (EntityP)
import Database.Generic.Entity qualified as Entity
import Database.Generic.Entity.EntityName (EntityName(..))
import Database.Generic.Entity.FieldName (FieldName)
import Database.Generic.Entity.SqlFields (HasSqlFields(..))
import Database.Generic.Entity.SqlTypes (SqlTypeId)
import Database.Generic.Prelude
import Database.Generic.Serialize (Serialize(..))

-- | Create a table for values of type 'a'.
data CreateTable a = CreateTable
  { fields      :: ![CreateTableColumn]
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
    , intercalate ", " $ c.fields <&> \c' -> unwords
          [ from c'.name
          , serialize @_ @db c'.type'
          , if c'.primary then "PRIMARY KEY" else ""
          ]
    , ");"
    ]

createTable :: forall a f b. EntityP f a b => Bool -> CreateTable a
createTable ifNotExists = do
  let primaryName = Entity.primaryKeyFieldName @a
  let fields = sqlFields @a <&> \(name, type') ->
        CreateTableColumn { primary = name == primaryName, .. }
  CreateTable { fields, name = Entity.entityName @_ @a, .. }
