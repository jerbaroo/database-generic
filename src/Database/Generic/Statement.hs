module Database.Generic.Statement where

import Database.Generic.Entity.SqlTypes (SqlTypeId, SqlValue(..))
import Database.Generic.Prelude
import Database.Generic.Statement.CreateTable qualified as C
import Database.Generic.Statement.Delete qualified as D
import Database.Generic.Statement.Insert qualified as I
import Database.Generic.Statement.Output (HasOutputType(..))
import Database.Generic.Statement.Select qualified as S
import Database.Generic.Statement.Tx qualified as Tx
import Database.Generic.Statement.Type (Cons, StatementType(..))
import Database.Generic.Serialize (Serialize(..))

data Statement (s :: [StatementType]) where
  StatementBeginTx     :: !Tx.BeginTx        -> Statement '[BeginTx]
  StatementCommitTx    :: !Tx.CommitTx       -> Statement '[CommitTx]
  StatementCreateTable :: !(C.CreateTable a) -> Statement '[CreateTable a]
  StatementDelete      :: !(D.Delete o a)    -> Statement '[Delete o a]
  StatementInsert      :: !(I.Insert o a)    -> Statement '[Insert o a]
  StatementSelect      :: !(S.Select o fs a) -> Statement '[Select o fs a]
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
  serialize (Cons             s2 s1) =
    serialize @_ @db s1 <> serialize @_ @db s2

-- | Typeclass to lift individual statements into 'Statement'.
class ToStatement s where
  type S s :: [StatementType]
  statement :: s -> Statement (S s)

instance ToStatement Tx.CommitTx where
  type S Tx.CommitTx = '[CommitTx]
  statement = StatementCommitTx

instance ToStatement (C.CreateTable (a :: Type)) where
  type S (C.CreateTable a) = '[CreateTable a]
  statement = StatementCreateTable

instance ToStatement (D.Delete o (a :: Type)) where
  type S (D.Delete o a) = '[Delete o a]
  statement = StatementDelete

instance ToStatement (I.Insert o (a :: Type)) where
  type S (I.Insert o a) = '[Insert o a]
  statement = StatementInsert

instance ToStatement (S.Select o (fs :: Type) (a :: Type)) where
  type S (S.Select o fs a) = '[Select o fs a]
  statement = StatementSelect

-- | Append a commit statement to a 'Statement'.
commitTx :: Statement s -> Statement (Cons CommitTx s)
commitTx = Cons $ StatementCommitTx Tx.CommitTx
