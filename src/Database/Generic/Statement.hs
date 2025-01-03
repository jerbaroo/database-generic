{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Generic.Statement where

import Database.Generic.Entity (Entity)
import Database.Generic.Entity qualified as Entity
import Database.Generic.Entity.SqlTypes (SqlTypeId, SqlValue(..))
import Database.Generic.Entity.ToSql (ToSqlValue(toSqlValue))
import Database.Generic.Output (HasOutputType, OutputType, Returning(..))
import Database.Generic.Output qualified as Output
import Database.Generic.Prelude
import Database.Generic.Serialize (Serialize(..))
import Database.Generic.Table (TableName)

-- * Single Statement

data Statement (r :: Returning a) where
  StatementBeginTx     ::                     Statement Nada
  StatementCommitTx    ::                     Statement Nada
  StatementCreateTable :: !(CreateTable a) -> Statement Nada
  StatementDelete      :: !(Delete q)      -> Statement q
  StatementInsert      :: !(Insert q)      -> Statement q
  StatementSelect      :: !(Select q)      -> Statement q

instance
  ( Serialize SqlTypeId db
  , Serialize SqlValue db
  , Serialize (CreateTable a) db
  , Serialize (Delete r) db
  , Serialize (Select r) db
  ) => Serialize (Statement r) db where
  serialize StatementBeginTx                  = "BEGIN TRANSACTION;" -- TODO by db
  serialize StatementCommitTx                 = "COMMIT TRANSACTION;" -- TODO by db
  serialize (StatementCreateTable s) = serialize @_ @db s
  serialize (StatementDelete      s) = serialize @_ @db s
  serialize (StatementInsert      s) = serialize @_ @db s
  serialize (StatementSelect      s) = serialize @_ @db s

-- | A statement without return type information.
data StatementX where
  StatementX :: Statement r -> StatementX

instance
  ( Serialize SqlTypeId db
  , Serialize SqlValue db
  ) => Serialize StatementX db where
  serialize (StatementX s) = serialize @_ @db s

-- * Combined Statements

-- | A sequence of SQL statements.
newtype Statements (r :: Returning a) =
  Statements ([StatementX], Statement r, [StatementX])

instance
  ( Serialize SqlTypeId db
  , Serialize SqlValue db
  ) => Serialize (Statements r) db where
  serialize (Statements (as, b, cs)) =
    unwords $ serialize @_ @db <$> (as <> [StatementX b] <> cs)

statements :: Statement r -> Statements r
statements = Statements . ([],,[])

commitTx :: Statements r -> Statements r
commitTx (Statements (as, b, cs)) =
  Statements (as, b, cs <> [StatementX StatementCommitTx])

outputType :: forall a r. HasOutputType r =>
  Statements (r :: Returning a) -> OutputType
outputType _ = Output.outputType @r

-- * Create Table

-- | Create a table for values of type 'a'.
data CreateTable a = CreateTable
  { ifNotExists :: !Bool
  , name        :: !TableName
  , columns     :: ![CreateTableColumn]
  }

data CreateTableColumn = CreateTableColumn
  { name    :: !String
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
          [ c'.name
          , serialize @_ @db c'.type'
          , if c'.primary then "PRIMARY KEY" else ""
          ]
    , ");"
    ]

createTable' :: forall a f. Entity f a => Bool -> CreateTable a
createTable' ifNotExists = do
  let primaryName = Entity.primaryKeyFieldName @_ @a
  let columns =
        zip (Entity.sqlFieldNames @_ @a) (Entity.sqlFieldTypes @_ @a) <&>
          \(name, type') -> CreateTableColumn
            { primary = name == primaryName, .. }
  CreateTable { name = Entity.tableName @_ @a, columns, .. }

createTable :: forall a f. Entity f a => Bool -> Statements Nada
createTable = statements . StatementCreateTable . createTable' @a

-- * Delete By ID

-- | Delete one or many values from table 'a'.
data Delete (r :: Returning a) = Delete
  { from   :: !TableName
  , where' :: !Wheres
  }

instance Serialize SqlValue db => Serialize (Delete r) db where
  serialize d = unwords
    [ "DELETE FROM", serialize d.from, "WHERE", serialize @_ @db d.where', ";"]

deleteById' :: forall a f b.
  (Entity f a, HasField f a b, ToSqlValue b) => b -> Delete (OneAffected a)
deleteById' b = Delete
  { from   = Entity.tableName @_ @a
  , where' = Wheres [idEquals @a b]
  }

deleteById :: forall a f b.
  (Entity f a, HasField f a b, ToSqlValue b) => b -> Statements (OneAffected a)
deleteById = statements . StatementDelete . deleteById'

-- * Insert

-- | Insert one or many values of type 'a' into table 't'.
data Insert (r :: Returning a) = Insert
  { table   :: !TableName
  , columns :: ![String]
  , rows    :: ![Values]
  }

instance Serialize SqlValue db => Serialize (Insert r) db where
  serialize i = unwords
    [ "INSERT INTO", serialize i.table
    , "(", intercalate ", " i.columns, ") VALUES"
    , intercalate ", " $ serialize @_ @db <$> i.rows
    , ";"
    ]

insertOne' :: forall a f. Entity f a => a -> Insert (OneAffected a)
insertOne' a = Insert
  { table   = Entity.tableName @_ @a
  , columns = Entity.sqlFieldNames @_ @a
  , rows    = [Values $ Entity.toSqlValues a]
  }

insertOne :: forall a f. Entity f a => a -> Statements (OneAffected a)
insertOne = statements . StatementInsert . insertOne'

-- * Select

-- | Select one or many values of type 'a' from table 't'.
data Select (r :: Returning a) = Select
  { from   :: !TableName
  , where' :: !Wheres
  }

instance Serialize SqlValue db => Serialize (Select r) db where
  serialize s = unwords
    ["SELECT * FROM", serialize s.from, "WHERE", serialize @_ @db s.where', ";"]

selectById' :: forall a f b.
  (Entity f a, HasField f a b, ToSqlValue b) => b -> Select (MaybeOne a)
selectById' b = Select
  { from   = Entity.tableName @_ @a
  , where' = Wheres [idEquals @a b]
  }

selectById :: forall a f b.
  (Entity f a, HasField f a b, ToSqlValue b) => b -> Statements (MaybeOne a)
selectById = statements . StatementSelect . selectById'

-- * Where

newtype Where = Equals (String, SqlValue)

idEquals :: forall a f b.
  (Entity f a, HasField f a b, ToSqlValue b) => b -> Where
idEquals b = Equals (Entity.primaryKeyFieldName @_ @a, toSqlValue b)

instance Serialize SqlValue db => Serialize Where db where
  serialize (Equals (column, value)) =
    column <> " = " <> serialize @_ @db value

newtype Wheres = Wheres [Where]

instance Serialize SqlValue db => Serialize Wheres db where
  serialize (Wheres ws) = intercalate " AND " $ serialize @_ @db <$> ws

-- * Values

newtype Values = Values [SqlValue]

instance Serialize SqlValue db => Serialize Values db where
  serialize (Values vs) = "(" <> intercalate "," (serialize @_ @db <$> vs) <> ")"
