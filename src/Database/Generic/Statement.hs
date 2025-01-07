module Database.Generic.Statement where

import Database.Generic.Entity.SqlTypes (SqlTypeId, SqlValue(..))
import Database.Generic.Statement.CreateTable (CreateTable)
import Database.Generic.Statement.Delete (Delete)
import Database.Generic.Statement.Insert (Insert)
import Database.Generic.Statement.Output (HasOutputType(..))
import Database.Generic.Statement.Select (Select)
import Database.Generic.Statement.Tx qualified as Tx
import Database.Generic.Statement.Returning (StatementType(..))
import Database.Generic.Prelude
import Database.Generic.Serialize (Serialize(..))

-- | Sum type of the various statements.
data Statement (s :: StatementType) where
  StatementBeginTx     :: !Tx.BeginTx      -> Statement Nada
  StatementCommitTx    :: !Tx.CommitTx     -> Statement Nada
  StatementCreateTable :: !(CreateTable a) -> Statement Nada
  StatementDelete      :: !(Delete x)      -> Statement x
  StatementInsert      :: !(Insert x)      -> Statement x
  StatementSelect      :: !(Select x)      -> Statement x
  Statements           :: ![StatementE]    -> Statement x
  StatementX           :: String           -> Statement x

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
  serialize (Statements          ss) = unwords $ serialize @_ @db <$> ss
  serialize (StatementX           s) = s

-- | Typeclass to lift various statements into 'Statement'.
class ToStatement s where
  type R s :: StatementType
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

-- | Return type of a statement after a 'CommitTx' is appended.
type        CommitType :: StatementType -> StatementType
type family CommitType a where
  CommitType (MaybeOne a) = MaybeOne a
  CommitType _            = Nada

-- class AppendCommit s1 s2 where

-- | Append a commit statement to a 'Statement'.
commitTx :: Statement r -> Statement (CommitType r)
commitTx (Statements ss) = undefined
  -- Statements (as <> [StatementE b], StatementCommitTx CommitTx)
commitTx s = commitTx $ toStatements s

toStatements :: Statement r -> Statement r
toStatements s@Statements{} = s
toStatements s              = undefined -- Statements ([], s)
