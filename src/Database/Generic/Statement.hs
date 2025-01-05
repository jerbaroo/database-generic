module Database.Generic.Statement where

import Database.Generic.Entity (Entity)
import Database.Generic.Entity.SqlTypes (SqlTypeId, SqlValue(..))
import Database.Generic.Entity.ToSql (ToSqlValue)
import Database.Generic.Statement.CreateTable (CreateTable)
import Database.Generic.Statement.CreateTable qualified as CreateTable
import Database.Generic.Statement.Delete (Delete)
import Database.Generic.Statement.Delete qualified as Delete
import Database.Generic.Statement.Insert (Insert)
import Database.Generic.Statement.Insert qualified as Insert
import Database.Generic.Statement.Output (HasOutputType(..))
import Database.Generic.Statement.Select (Select)
import Database.Generic.Statement.Select qualified as Select
import Database.Generic.Statement.Returning (Returning(..))
import Database.Generic.Prelude
import Database.Generic.Serialize (Serialize(..))

-- | Sum type of the various statements.
data Statement r where
  StatementBeginTx     ::                                               Statement Nada
  StatementCommitTx    ::                                               Statement Nada
  StatementCreateTable :: !(CreateTable a)                           -> Statement Nada
  StatementDelete      :: !(Delete x)                                -> Statement x
  StatementInsert      :: !(Insert x)                                -> Statement x
  StatementSelect      :: !(Select x)                                -> Statement x
  Statements           :: !([StatementE], Statement x, [StatementE]) -> Statement x
  StatementX           :: String                                     -> Statement x

instance HasOutputType r => HasOutputType (Statement r) where
  outputType = outputType @r

instance
  ( Serialize SqlTypeId db
  , Serialize SqlValue db
  ) => Serialize (Statement r) db where
  serialize StatementBeginTx         = "BEGIN TRANSACTION;" -- TODO by db
  serialize StatementCommitTx        = "COMMIT TRANSACTION;" -- TODO by db
  serialize (StatementCreateTable s) = serialize @_ @db s
  serialize (StatementDelete      s) = serialize @_ @db s
  serialize (StatementInsert      s) = serialize @_ @db s
  serialize (StatementSelect      s) = serialize @_ @db s
  serialize (Statements (as, b, cs)) =
    unwords $ serialize @_ @db <$> (as <> [StatementE b] <> cs)

-- | A statement without return type information.
data StatementE = forall r. StatementE (Statement r)

instance
  ( Serialize SqlTypeId db
  , Serialize SqlValue db
  ) => Serialize StatementE db where
  serialize (StatementE s) = serialize @_ @db s

-- | Append a commit statement to a 'Statement'.
commitTx :: Statement r -> Statement r
commitTx (Statements (as, b, cs)) =
  Statements (as, b, cs <> [StatementE StatementCommitTx])
commitTx s = commitTx $ toStatements s

toStatements :: Statement r -> Statement r
toStatements s@Statements{} = s
toStatements s              = Statements ([], s, [])

-- * Convenience wrappers.

createTable :: forall a f. Entity f a => Bool -> Statement Nada
createTable = StatementCreateTable . CreateTable.createTable @a

deleteById :: forall a f b.
  (Entity f a, HasField f a b, ToSqlValue b) => b -> Statement (OneAffected a)
deleteById = StatementDelete . Delete.deleteById

insertOne :: forall a f. Entity f a => a -> Statement (OneAffected a)
insertOne = StatementInsert . Insert.insertOne

selectById :: forall a f b.
  (Entity f a, HasField f a b, ToSqlValue b) => b -> Statement (MaybeOne a)
selectById = StatementSelect . Select.selectById
