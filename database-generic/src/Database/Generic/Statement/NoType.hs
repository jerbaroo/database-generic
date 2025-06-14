{-# LANGUAGE UndecidableInstances #-}

module Database.Generic.Statement.NoType where

import Data.Aeson qualified as Aeson
import Database.Generic.Entity.SqlTypes (DbType, DbValue)
import Database.Generic.Prelude
import Database.Generic.Statement qualified as S
import Database.Generic.Statement.CreateTable qualified as C
import Database.Generic.Statement.Delete qualified as C
import Database.Generic.Statement.Insert qualified as C
import Database.Generic.Statement.Select qualified as C
import Database.Generic.Statement.Tx qualified as Tx
import Database.Generic.Serialize (Serialize(..))

-- | Similar to 'Database.Generic.Statement.Statement' but without type info.
--
-- The use case for this is when no longer want to make modifications to the
-- statement (type info needed for that), and we are ready to send the statement
-- to 'MonadDb' to be executed.
data Statement where
  StatementBeginTx     :: !Tx.BeginTx     -> Statement
  StatementCommitTx    :: !Tx.CommitTx    -> Statement
  StatementCreateTable :: !C.CreateTable' -> Statement
  StatementDelete      :: !C.Delete'      -> Statement
  StatementInsert      :: !C.Insert'      -> Statement
  StatementSelect      :: !C.Select'      -> Statement
  Cons                 :: !Statement      -> Statement -> Statement
  deriving (Generic)

instance Aeson.FromJSON Statement

instance From (S.Statement s) Statement where
  from (S.StatementBeginTx s) = StatementBeginTx s
  from (S.StatementCommitTx s) = StatementCommitTx s
  from (S.StatementCreateTable s) = StatementCreateTable $ from s
  from (S.StatementDelete s) = StatementDelete $ from s
  from (S.StatementInsert s) = StatementInsert $ from s
  from (S.StatementSelect s) = StatementSelect $ from s
  from (S.Cons s1 s2) = Cons (from s1) (from s2)

instance
  ( Serialize DbType db
  , Serialize DbValue db
  ) => Serialize Statement db where
  serialize (StatementBeginTx     s) = serialize @_ @db s
  serialize (StatementCommitTx    s) = serialize @_ @db s
  serialize (StatementCreateTable s) = serialize @_ @db s
  serialize (StatementDelete      s) = serialize @_ @db s
  serialize (StatementInsert      s) = serialize @_ @db s
  serialize (StatementSelect      s) = serialize @_ @db s
  serialize (Cons             s2 s1) =
    serialize @_ @db s1 <> serialize @_ @db s2
