module Database.Generic.Statement.CreateTable where

import Data.Aeson qualified as Aeson
import Database.Generic.Entity.EntityName (EntityName(..), entityName, HasEntityName)
import Database.Generic.Entity.FieldName (FieldName)
import Database.Generic.Entity.PrimaryKey (primaryKeyFieldName, PrimaryKey)
import Database.Generic.Entity.SqlColumns (HasDbColumns(..))
import Database.Generic.Prelude
import Database.Generic.Serialize (Serialize(..))
import Database.Generic.Serialize qualified as Serialize

-- | Create a table for values of type 'a'.
newtype CreateTable a dbt = CreateTable (CreateTable' dbt)
  deriving (Eq, From (CreateTable' dbt), Show)

instance From (CreateTable a dbt) (CreateTable' dbt)

-- | 'CreateTable' without type info.
data CreateTable' dbt = CreateTable'
  { columns     :: ![CreateTableColumn dbt]
  , ifNotExists :: !Bool
  , name        :: !EntityName
  } deriving (Eq, Generic, Show)

instance Aeson.FromJSON dbt => Aeson.FromJSON (CreateTable' dbt)

data CreateTableColumn dbt = CreateTableColumn
  { name    :: !FieldName
  , primary :: !Bool
  , type'   :: !dbt
  } deriving (Eq, Generic, Show)

instance Aeson.FromJSON dbt => Aeson.FromJSON (CreateTableColumn dbt)

instance Serialize dbt db => Serialize (CreateTable' dbt) db where
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

createTable :: forall a f dbt.
  (HasDbColumns dbt a, HasEntityName a, PrimaryKey f a)
  => Bool -> CreateTable a dbt
createTable ifNotExists = do
  let primaryName = primaryKeyFieldName @a
  let columns = sqlColumns @dbt @a <&> \(name, type') ->
        CreateTableColumn { primary = name == primaryName, .. }
  CreateTable $ CreateTable' { columns, name = entityName @a, .. }
