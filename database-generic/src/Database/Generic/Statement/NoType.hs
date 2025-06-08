module Database.Generic.Statement.NoType where

import Database.Generic.Prelude
import Database.Generic.Statement qualified as S
import Database.Generic.Statement.CreateTable qualified as C
import Database.Generic.Statement.Tx qualified as Tx

-- | Similar to 'Database.Generic.Statement.Statement' but without type info.
--
-- The use case for this is when no longer want to make modifications to the
-- statement (type info needed for that), and we are ready to send the statement
-- to 'MonadDb' to be executed.
data Statement where
  StatementBeginTx     :: !Tx.BeginTx     -> Statement
  StatementCommitTx    :: !Tx.CommitTx    -> Statement
  StatementCreateTable :: !C.CreateTable' -> Statement

instance From (S.Statement s) Statement where
  from (S.StatementBeginTx a) = StatementBeginTx a
  from (S.StatementCommitTx a) = StatementCommitTx a
  from (S.StatementCreateTable a) = StatementCreateTable $ from a
