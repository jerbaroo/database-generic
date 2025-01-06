module Database.Generic.Statement where

import Database.Generic.Entity.SqlTypes (SqlTypeId, SqlValue(..))
import Database.Generic.Statement.CreateTable (CreateTable)
import Database.Generic.Statement.Delete (Delete)
import Database.Generic.Statement.Insert (Insert)
import Database.Generic.Statement.Output (HasOutputType(..))
import Database.Generic.Statement.Select (Select)
import Database.Generic.Statement.Tx (BeginTx, CommitTx(..))
import Database.Generic.Statement.Returning (Returning(..))
import Database.Generic.Prelude
import Database.Generic.Serialize (Serialize(..))

-- | Sum type of the various statements.
data Statement r where
  StatementBeginTx     :: !BeginTx                                   -> Statement Nada
  StatementCommitTx    :: !CommitTx                                  -> Statement Nada
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
  serialize (StatementBeginTx     s) = serialize @_ @db s
  serialize (StatementCommitTx    s) = serialize @_ @db s
  serialize (StatementCreateTable s) = serialize @_ @db s
  serialize (StatementDelete      s) = serialize @_ @db s
  serialize (StatementInsert      s) = serialize @_ @db s
  serialize (StatementSelect      s) = serialize @_ @db s
  serialize (Statements (as, b, cs)) =
    unwords $ serialize @_ @db <$> (as <> [StatementE b] <> cs)
  serialize (StatementX           s) = s

-- | Typeclass to lift various statements into 'Statement'.
class ToStatement s where
  type R s :: Returning
  statement :: s -> Statement (R s)

instance ToStatement (CreateTable a) where
  type R (CreateTable a) = Nada
  statement = StatementCreateTable

instance ToStatement (Delete r) where
  type R (Delete r) = r
  statement = StatementDelete

instance ToStatement (Insert r) where
  type R (Insert r) = r
  statement = StatementInsert

instance ToStatement (Select r) where
  type R (Select r) = r
  statement = StatementSelect

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
  Statements (as, b, cs <> [StatementE $ StatementCommitTx CommitTx])
commitTx s = commitTx $ toStatements s

toStatements :: Statement r -> Statement r
toStatements s@Statements{} = s
toStatements s              = Statements ([], s, [])
