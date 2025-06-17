module Database.Generic.Statement where

import Database.Generic.Prelude
import Database.Generic.Statement.CreateTable qualified as C
import Database.Generic.Statement.Delete qualified as D
import Database.Generic.Statement.Insert qualified as I
import Database.Generic.Statement.Output (HasOutputType(..))
import Database.Generic.Statement.Select qualified as S
import Database.Generic.Statement.Tx qualified as Tx
import Database.Generic.Statement.Type (List(..), StatementType(..))

-- | One or more statements. We track a type-level list 's' of the statements,
-- because the order of the statements can affect what is returned.
data Statement (s :: List StatementType) where
  StatementBeginTx     :: !Tx.BeginTx          -> Statement (One BeginTx)
  StatementCommitTx    :: !Tx.CommitTx         -> Statement (One CommitTx)
  StatementCreateTable :: !(C.CreateTable a)   -> Statement (One (CreateTable a))
  StatementDelete      :: !(D.Delete o r a)    -> Statement (One (Delete o r a))
  StatementInsert      :: !(I.Insert o r a)    -> Statement (One (Insert o r a))
  StatementSelect      :: !(S.Select o r a ob) -> Statement (One (Select o r a ob))
  Cons                 :: !(Statement (One s1))  -> !(Statement s2) -> Statement (L s1 s2)

instance HasOutputType r => HasOutputType (Statement r) where
  outputType = outputType @r

-- | Typeclass to lift individual statements into 'Statement'.
class ToStatement s where
  type S s :: List StatementType
  statement :: s -> Statement (S s)

instance ToStatement Tx.CommitTx where
  type S Tx.CommitTx = One CommitTx
  statement = StatementCommitTx

instance ToStatement (C.CreateTable (a :: Type)) where
  type S (C.CreateTable a) = One (CreateTable a)
  statement = StatementCreateTable

instance ToStatement (D.Delete o (r :: Maybe fs) (a :: Type)) where
  type S (D.Delete o r a) = One (Delete o r a)
  statement = StatementDelete

instance ToStatement (I.Insert o (r :: Maybe fs) (a :: Type)) where
  type S (I.Insert o r a) = One (Insert o r a)
  statement = StatementInsert

instance ToStatement (S.Select o (r :: Type) (a :: Type) (ob :: Bool)) where
  type S (S.Select o r a ob) = One (Select o r a ob)
  statement = StatementSelect

-- | Append a commit statement to a 'Statement'.
commitTx :: Statement s -> Statement (L CommitTx s)
commitTx = Cons $ StatementCommitTx Tx.CommitTx
