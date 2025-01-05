{-# LANGUAGE UndecidableInstances #-}

module Database.Generic.Statement where

import Database.Generic.Entity.SqlTypes (SqlTypeId, SqlValue(..))
import Database.Generic.Statement.CreateTable (CreateTable)
import Database.Generic.Statement.Delete (Delete)
import Database.Generic.Statement.Insert (Insert)
import Database.Generic.Statement.Output (HasOutputType, OutputType)
import Database.Generic.Statement.Output qualified as Output
import Database.Generic.Statement.Select (Select)
import Database.Generic.Statement.Returning (Returning(..))
import Database.Generic.Prelude
import Database.Generic.Serialize (Serialize(..))

-- * Single Statement

data Statement (r :: Returning a) where
  StatementBeginTx     ::                     Statement Nada
  StatementCommitTx    ::                     Statement Nada
  StatementCreateTable :: !(CreateTable a) -> Statement Nada
  StatementDelete      :: !(Delete q)      -> Statement q
  StatementInsert      :: !(Insert q)      -> Statement q
  StatementSelect      :: !(Select q)      -> Statement q

instance From (Select q) (Statement q) where
  from = StatementSelect

instance
  ( Serialize SqlTypeId db
  , Serialize SqlValue db
  , Serialize (CreateTable a) db
  , Serialize (Delete r) db
  , Serialize (Select r) db
  ) => Serialize (Statement r) db where
  serialize StatementBeginTx         = "BEGIN TRANSACTION;" -- TODO by db
  serialize StatementCommitTx        = "COMMIT TRANSACTION;" -- TODO by db
  serialize (StatementCreateTable s) = serialize @_ @db s
  serialize (StatementDelete      s) = serialize @_ @db s
  serialize (StatementInsert      s) = serialize @_ @db s
  serialize (StatementSelect      s) = serialize @_ @db s

-- | A statement without return type information.
data StatementX where
  StatementX :: Statement r -> StatementX

instance
  ( Serialize SqlTypeId db
  , Serialize SqlValue db
  ) => Serialize StatementX db where
  serialize (StatementX s) = serialize @_ @db s

-- * Combined Statements

-- | A sequence of SQL statements.
newtype Statements (r :: Returning a) =
  Statements ([StatementX], Statement r, [StatementX])

instance
  ( Serialize SqlTypeId db
  , Serialize SqlValue db
  ) => Serialize (Statements r) db where
  serialize (Statements (as, b, cs)) =
    unwords $ serialize @_ @db <$> (as <> [StatementX b] <> cs)

statements :: Statement r -> Statements r
statements = Statements . ([],,[])

commitTx :: Statements r -> Statements r
commitTx (Statements (as, b, cs)) =
  Statements (as, b, cs <> [StatementX StatementCommitTx])

outputType :: forall a r. HasOutputType r =>
  Statements (r :: Returning a) -> OutputType
outputType _ = Output.outputType @r
