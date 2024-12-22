module Database.Generic.Statement where

import Database.Generic.Database (Database(..))
import Database.Generic.Entity (Entity)
import Database.Generic.Entity qualified as Entity
import Database.Generic.Entity.SqlTypes (SqlTypeId)
import Database.Generic.Prelude

class Database db => Serialize a db where
  serialize :: a -> String

-- * Statement

data Statement
  = StatementCreateTable CreateTable
  | BeginTx
  | CommitTx

instance (Database db, Serialize CreateTable db) => Serialize Statement db where
  serialize (StatementCreateTable s) = serialize @_ @db s
  serialize BeginTx                  = "BEGIN TRANSACTION;"
  serialize CommitTx                 = "COMMIT TRANSACTION;"

-- * Statements

newtype Statements = Statements [Statement]

instance Semigroup Statements where
  (Statements a) <> (Statements b) = Statements $ a <> b

instance (Database db, Serialize Statement db) => Serialize Statements db where
  serialize (Statements s) = intercalate "\n" $ serialize @_ @db <$> s

-- * Transaction

beginTx :: Statements
beginTx = Statements [BeginTx]

commitTx :: Statements
commitTx = Statements [CommitTx]

-- * Create Table

data CreateTable = CreateTable
  { ifNotExists :: Bool
  , name        :: String
  , columns     :: [CreateTableColumn]
  }

data CreateTableColumn = CreateTableColumn
  { name    :: !String
  , primary :: !Bool
  , type'   :: !SqlTypeId
  }

instance Database db => Serialize CreateTable db where
  serialize c = unwords
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

createTable :: forall a f. Entity f a => Bool -> Statements
createTable ifNotExists = do
  let primaryName = Entity.primaryKeyFieldName @_ @a
  let columns =
        zip (Entity.sqlFieldNames @_ @a) (Entity.sqlFieldTypes @_ @a) <&>
          \(name, type') -> CreateTableColumn
            { primary = name == primaryName, .. }
  Statements
    [ StatementCreateTable $ CreateTable
        { name = Entity.tableName @_ @a, columns, .. }
    ]
