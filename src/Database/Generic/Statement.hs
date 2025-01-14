module Database.Generic.Statement where

import Database.Generic.Entity.SqlTypes (SqlTypeId, SqlValue(..))
import Database.Generic.Prelude
import Database.Generic.Statement.CreateTable (CreateTable)
import Database.Generic.Statement.Delete (Delete)
import Database.Generic.Statement.Insert (Insert)
import Database.Generic.Statement.Output (HasOutputType(..))
import Database.Generic.Statement.Select (Select)
import Database.Generic.Statement.Tx qualified as Tx
import Database.Generic.Statement.Returning (Cons, StatementType(..))
import Database.Generic.Serialize (Serialize(..))

data Statement (s :: [StatementType]) where
  StatementBeginTx     :: !Tx.BeginTx        -> Statement '[BeginTx]
  StatementCommitTx    :: !Tx.CommitTx       -> Statement '[CommitTx]
  StatementCreateTable :: !(CreateTable a)   -> Statement '[Nada]
  StatementDelete      :: !(Delete x)        -> Statement '[x]
  StatementInsert      :: !(Insert x)        -> Statement '[x]
  StatementSelect      :: !(Select x)        -> Statement '[x]
  Cons                 :: !(Statement '[s1]) -> (Statement s2) -> Statement (Cons s1 s2)

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
  serialize (Cons             s1 s2) =
    serialize @_ @db s1 <> ";" <> serialize @_ @db s2

-- | Typeclass to lift individual statements into 'Statement'.
class ToStatement s where
  type S s :: [StatementType]
  statement :: s -> Statement (S s)

instance ToStatement Tx.CommitTx where
  type S Tx.CommitTx = '[CommitTx]
  statement = StatementCommitTx

instance ToStatement (CreateTable a) where
  type S (CreateTable a) = '[Nada]
  statement = StatementCreateTable

instance ToStatement (Delete r) where
  type S (Delete r) = '[r]
  statement = StatementDelete

instance ToStatement (Insert r) where
  type S (Insert r) = '[r]
  statement = StatementInsert

instance ToStatement (Select s) where
  type S (Select s) = '[s]
  statement = StatementSelect

-- | A statement without return type information.
data StatementE = forall r. StatementE (Statement r)

instance
  ( Serialize SqlTypeId db
  , Serialize SqlValue db
  ) => Serialize StatementE db where
  serialize (StatementE s) = serialize @_ @db s

-- | Append a commit statement to a 'Statement'.
commitTx :: Statement s -> Statement (Cons CommitTx s)
commitTx = Cons $ StatementCommitTx Tx.CommitTx
