module Database.Generic.Statement where

import Database.Generic.Entity (Entity)
import Database.Generic.Entity qualified as Entity
import Database.Generic.Entity.SqlTypes (SqlTypeId, SqlValue(..))
import Database.Generic.Entity.ToSql (ToSqlValue(toSqlValue))
import Database.Generic.Prelude
import Database.Generic.Serialize (Serialize(..), TableName, tableName)

-- * Statement

data Statement
  = BeginTx
  | CommitTx
  | StatementDelete      !Delete
  | StatementCreateTable !CreateTable

instance (Serialize CreateTable db, Serialize Delete db) => Serialize Statement db where
  serialize BeginTx                  = "BEGIN TRANSACTION;"
  serialize CommitTx                 = "COMMIT TRANSACTION;"
  serialize (StatementDelete      s) = serialize @_ @db s
  serialize (StatementCreateTable s) = serialize @_ @db s

-- * Statements

newtype Statements = Statements [Statement]

instance Semigroup Statements where
  (Statements a) <> (Statements b) = Statements $ a <> b

instance Serialize Statement db => Serialize Statements db where
  serialize (Statements s) = intercalate "\n" $ serialize @_ @db <$> s

-- * Transaction

beginTx :: Statements
beginTx = Statements [BeginTx]

commitTx :: Statements
commitTx = Statements [CommitTx]

-- * Create Table

data CreateTable = CreateTable
  { ifNotExists :: !Bool
  , name        :: !TableName
  , columns     :: ![CreateTableColumn]
  }

data CreateTableColumn = CreateTableColumn
  { name    :: !String
  , primary :: !Bool
  , type'   :: !SqlTypeId
  }

type S db = (Serialize SqlTypeId db, Serialize TableName db)

instance S db => Serialize CreateTable db where
  serialize c = unwords
    [ "CREATE TABLE"
    , if c.ifNotExists then "IF NOT EXISTS" else ""
    , serialize @_ @db c.name
    , "("
    , intercalate ", " $ c.columns <&> \c' -> unwords
          [ c'.name
          , serialize @_ @db c'.type'
          , if c'.primary then "PRIMARY KEY" else ""
          ]
    , ");"
    ]

createTable :: forall a f. Entity f a => Bool -> Statements
createTable ifNotExists = do
  let primaryName = Entity.primaryKeyFieldName @_ @a
  let columns =
        zip (Entity.sqlFieldNames @_ @a) (Entity.sqlFieldTypes @_ @a) <&>
          \(name, type') -> CreateTableColumn
            { primary = name == primaryName, .. }
  Statements [ StatementCreateTable $ CreateTable
    { name = tableName @a, columns, .. } ]

-- * Delete By ID

data Delete = Delete
  { table :: !String
  , id    :: !SqlValue
  , idCol :: !String
  }

instance S db => Serialize Delete db where
  serialize d = unwords
    [ "DELETE FROM", d.table, "WHERE", d.idCol, "=", serialize @_ @db d.id, ";"]

deleteById :: forall a f b.
  (ToSqlValue b, Entity f a, HasField f a b) => b -> Statements
deleteById b =
  Statements [ StatementDelete $ Delete
    { table = Entity.tableName @_ @a
    , id    = toSqlValue b
    , idCol = Entity.primaryKeyFieldName @_ @a
    } ]
