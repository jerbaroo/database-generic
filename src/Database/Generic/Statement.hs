{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Generic.Statement where

import Database.Generic.Entity (Entity)
import Database.Generic.Entity qualified as Entity
import Database.Generic.Entity.SqlTypes (SqlTypeId, SqlValue(..))
import Database.Generic.Entity.ToSql (ToSqlValue(toSqlValue))
import Database.Generic.Prelude
import Database.Generic.Serialize (Serialize(..))
import Database.Generic.Table (TableName)

-- | Types of values returned from SQL statements.
-- TODO One a
-- TODO MaybeOneAffected
data Returning a where
  MaybeOne    :: Type -> Returning a-- | Many !a | OneAffected !a | ManyAffected !a | Nada
  OneAffected :: Type -> Returning a
  Nada        :: Returning a

-- type ReturningF :: forall a b. Returning a -> b
-- type family ReturningF r where
--   ReturningF (MaybeOne    a) = Maybe a
--   ReturningF (OneAffected _) = ()

class ParseOutput r where
  type RetT r
  parse :: Output -> Either String (RetT r)

instance ParseOutput (MaybeOne a) where
  type RetT (MaybeOne a) = Maybe a
  parse (OutputRows [])    = Right Nothing
  parse (OutputRows [row]) = Right $ Just undefined
  parse output             = Left  $ "Expected 0 or 1 rows but got " <> show output

executeAndParse'
  :: forall m a r. (Functor m, ParseOutput r)
  => ExecuteStatement m a r
  -> Statement (r :: Returning a)
  -> m (Either String (RetT r))
executeAndParse' f s = f s <&> parse @r

-- * Output.

-- | Output of SQL statement before parsing into return value.
data Output
  = OutputAffected !Int
  | OutputRows ![[SqlValue]]
  | OutputNada
  deriving Show

-- | Types of output values from SQL statements.
data OutputType = OutputTypeAffected | OutputTypeRows | OutputTypeNada

class HasOutputType r where
  outputType :: OutputType

instance HasOutputType (MaybeOne a) where
  outputType = OutputTypeRows

class ExpectedAffected r where
  expectedAffected :: Int

-- | Execute a statement and gather the output.
type ExecuteStatement m a r = Statement (r :: Returning a) -> m Output

-- class StatementReturning a where
--   type Ret a
--   parse :: forall r.

-- executeAndParse
--   :: forall m a r. (ExpectedAffected r, HasOutputType r, Monad m)
--   => ExecuteStatement m a r
--   -> Statement (r :: Returning a)
--   -> m (Either String (ReturningF r))
-- executeAndParse f s = f s >>= \output -> case (outputType @r, output) of
--   (OutputTypeAffected, OutputAffected affected) -> do
--     if   expectedAffected @r /= affected
--     then pure $ Left $ "Expected " <> show (expectedAffected @r) <> " affected rows but got " <> show affected
--     else pure $ Left "Right ()"
--   (OutputTypeRows    , OutputRows     rows    ) -> pure $ Left "ok"
--   (OutputTypeAffected, _                      ) -> pure $ Left $ "Expected OutputAffected but got " <> show output
--   (OutputTypeRows    , _                      ) -> pure $ Left $ "Expected OutputRows but got " <> show output
--   (OutputTypeNada    , OutputNada             ) -> pure $ Left "ok"
--   (OutputTypeNada    , _                      ) -> pure $ Left $ "Expected OutputNada but got " <> show output

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

-- * Combined Statements

-- | A sequence of SQL statements.
newtype Statements (r :: Returning a) =
  Statements ([StatementX], Statement r, [StatementX])

statements :: Statement r -> Statements r
statements = Statements . ([],,[])

commitTx :: Statements r -> Statements r
commitTx (Statements (as, b, cs)) =
  Statements (as, b, cs <> [StatementX StatementCommitTx])

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

createTable :: forall a f. Entity f a => Bool -> CreateTable a
createTable ifNotExists = do
  let primaryName = Entity.primaryKeyFieldName @_ @a
  let columns =
        zip (Entity.sqlFieldNames @_ @a) (Entity.sqlFieldTypes @_ @a) <&>
          \(name, type') -> CreateTableColumn
            { primary = name == primaryName, .. }
  CreateTable { name = Entity.tableName @_ @a, columns, .. }

-- * Delete By ID

-- | Delete one or many values from table 'a'.
data Delete (r :: Returning a) = Delete
  { from   :: !TableName
  , where' :: !Wheres
  }

instance Serialize (Delete r) db where
  serialize d = unwords
    [ "DELETE FROM", serialize d.from, "WHERE", serialize d.where', ";"]

deleteById :: forall a f b.
  (Entity f a, HasField f a b, ToSqlValue b) => b -> Delete (OneAffected a)
deleteById b = Delete
  { from   = Entity.tableName @_ @a
  , where' = Wheres [idEquals @a b]
  }

-- * Insert

-- | Insert one or many values of type 'a' into table 't'.
data Insert (r :: Returning a) = Insert
  { table   :: !TableName
  , columns :: ![String]
  , rows    :: ![Values]
  }

instance Serialize (Insert r) db where
  serialize i = unwords
    [ "INSERT INTO", serialize i.table
    , "(", intercalate ", " i.columns, ") VALUES"
    , intercalate ", " $ serialize <$> i.rows
    , ";"
    ]

insertOne :: forall a f. Entity f a => a -> Insert (OneAffected a)
insertOne a = Insert
  { table   = Entity.tableName @_ @a
  , columns = Entity.sqlFieldNames @_ @a
  , rows    = [Values $ Entity.toSqlValues a]
  }

-- * Select

-- | Select one or many values of type 'a' from table 't'.
data Select (r :: Returning a) = Select
  { from   :: !TableName
  , where' :: !Wheres
  }

instance Serialize (Select r) db where
  serialize s = unwords
    ["SELECT FROM", serialize s.from, "WHERE", serialize s.where', ";"]

selectById :: forall a f b.
  (Entity f a, HasField f a b, ToSqlValue b) => b -> Select (MaybeOne a)
selectById b = Select
  { from   = Entity.tableName @_ @a
  , where' = Wheres [idEquals @a b]
  }

-- * Where

newtype Where = Equals (String, SqlValue)

idEquals :: forall a f b.
  (Entity f a, HasField f a b, ToSqlValue b) => b -> Where
idEquals b = Equals (Entity.primaryKeyFieldName @_ @a, toSqlValue b)

instance Serialize Where db where
  serialize (Equals (column, value)) =
    column <> " = " <> serialize @_ @db value

newtype Wheres = Wheres [Where]

instance Serialize Wheres db where
  serialize (Wheres ws) = intercalate " AND " $ serialize <$> ws

-- * Values

newtype Values = Values [SqlValue]

instance Serialize Values db where
  serialize (Values vs) = "(" <> intercalate "," (serialize @_ @db <$> vs) <> ")"
