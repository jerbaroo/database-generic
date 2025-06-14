module Database.Generic.Statement where

import Database.Generic.Prelude
import Database.Generic.Statement.CreateTable qualified as C
import Database.Generic.Statement.Delete qualified as D
import Database.Generic.Statement.Insert qualified as I
import Database.Generic.Statement.Output (HasOutputType(..))
import Database.Generic.Statement.Select qualified as S
import Database.Generic.Statement.Tx qualified as Tx
import Database.Generic.Statement.Type (Cons, StatementType(..))

-- | One or more statements. We track a type-level list 's' of the statements,
-- because the order of the statements can affect what is returned.
data Statement (s :: [StatementType]) dbt where
  StatementBeginTx     :: !Tx.BeginTx          -> Statement '[BeginTx] dbt
  StatementCommitTx    :: !Tx.CommitTx         -> Statement '[CommitTx] dbt
  StatementCreateTable :: !(C.CreateTable dbt a)   -> Statement '[CreateTable a] dbt
  StatementDelete      :: !(D.Delete o r a)    -> Statement '[Delete o r a] dbt
  StatementInsert      :: !(I.Insert o r a)    -> Statement '[Insert o r a] dbt
  StatementSelect      :: !(S.Select o r a ob) -> Statement '[Select o r a ob] dbt
  Cons                 :: !(Statement '[s1] dbt)   -> (Statement s2 dbt) -> Statement (Cons s1 s2) dbt

instance HasOutputType r => HasOutputType (Statement r) where
  outputType = outputType @r

-- | Typeclass to lift individual statements into 'Statement'.
class ToStatement s where
  type S s :: [StatementType]
  statement :: s -> Statement (S s) dbt

instance ToStatement Tx.CommitTx where
  type S Tx.CommitTx = '[CommitTx]
  statement = StatementCommitTx

instance ToStatement (C.CreateTable (a :: Type)) where
  type S (C.CreateTable a) = '[CreateTable a]
  statement = StatementCreateTable

instance ToStatement (D.Delete o (r :: Maybe fs) (a :: Type)) where
  type S (D.Delete o r a) = '[Delete o r a]
  statement = StatementDelete

instance ToStatement (I.Insert o (r :: Maybe fs) (a :: Type)) where
  type S (I.Insert o r a) = '[Insert o r a]
  statement = StatementInsert

instance ToStatement (S.Select o (r :: Type) (a :: Type) (ob :: Bool)) where
  type S (S.Select o r a ob) = '[Select o r a ob]
  statement = StatementSelect

-- | Append a commit statement to a 'Statement'.
commitTx :: Statement s -> Statement (Cons CommitTx s)
commitTx = Cons $ StatementCommitTx Tx.CommitTx
