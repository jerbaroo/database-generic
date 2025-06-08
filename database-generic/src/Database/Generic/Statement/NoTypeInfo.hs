module Database.Generic.Statement.NoTypeInfo where

import Database.Generic.Statement.CreateTable qualified as C
import Database.Generic.Statement.Tx qualified as Tx

-- | Similar to 'Database.Generic.Statement.Statement' but without type info.
-- The use case for this is when no longer want to make modifications to the
-- statement (type info needed), and we want to send the statement over the
-- network to be executed.
data Statement where
  StatementBeginTx     :: !Tx.BeginTx     -> Statement
  StatementCommitTx    :: !Tx.CommitTx    -> Statement
  StatementCreateTable :: !C.CreateTable' -> Statement
