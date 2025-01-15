{-# LANGUAGE UndecidableInstances #-}

module Database.Generic.Statement.Output where

import Database.Generic.Prelude
import Database.Generic.Entity.SqlTypes (SqlValue(..))
import Database.Generic.Entity.FromSql (FromSqlValues(..))
import Database.Generic.Statement.Delete qualified as D
import Database.Generic.Statement.Insert qualified as I
import Database.Generic.Statement.Select qualified as S
import Database.Generic.Statement.Type (StatementType(..))

-- | Output from executing an SQL statement.
data Output
  = OutputAffected !Integer
  | OutputNada
  | OutputRows ![[SqlValue]]
  deriving Show

-- | The different types of output from executing SQL statements.
--
-- These constructors correspond to the constructors of 'Output'.
data OutputType = OutputTypeAffected | OutputTypeNada | OutputTypeRows

-- | The type of output expected from executing a statement of type 's'.
class HasOutputType s where
  outputType :: OutputType

instance HasOutputType BeginTx where
  outputType = OutputTypeNada

instance HasOutputType CommitTx where
  outputType = OutputTypeNada

instance HasOutputType (CreateTable a) where
  outputType = OutputTypeNada

instance HasOutputType (Delete o Nothing a) where
  outputType = OutputTypeAffected

instance HasOutputType (Delete o (Just fs) a) where
  outputType = OutputTypeRows

instance HasOutputType (Insert o a) where
  outputType = OutputTypeAffected

instance HasOutputType (Select o fs a) where
  outputType = OutputTypeRows

type Head :: forall a. [a] -> a
type family Head xs where
  Head '[a]   = a
  Head (a:as) = a

instance HasOutputType (Head s) => HasOutputType (s :: [StatementType]) where
  outputType = outputType @(Head s)

data OutputError
  = ExpectedMaybeOne         !Output
  | ExpectedMaybeOneAffected !Output
  | ExpectedNada             !Output
  | ExpectedOneAffected      !Output
  | ExpectedOutputAffected   !Output
  deriving Show

instance Exception OutputError where
  displayException (ExpectedMaybeOne         o) = "Expected 0 or 1 rows but got " <> show o
  displayException (ExpectedMaybeOneAffected o) = "Expected 0 or 1 affected but got " <> show o
  displayException (ExpectedNada             o) = "Expected OutputNada but got " <> show o
  displayException (ExpectedOneAffected      o) = "Expected one affected but got " <> show o
  displayException (ExpectedOutputAffected   o) = "Expected OutputAffected but got " <> show o

-- | Parse 'Output' into the return value expected from executing a statement.
class ParseOutput s where
  type OutputT s
  parse :: Output -> Either OutputError (OutputT s)

instance ParseOutput BeginTx where
  type OutputT BeginTx = ()
  parse OutputNada     = Right ()
  parse output         = Left $ ExpectedNada output

instance ParseOutput CommitTx where
  type OutputT CommitTx = ()
  parse OutputNada      = Right ()
  parse output          = Left $ ExpectedNada output

instance ParseOutput (CreateTable a) where
  type OutputT (CreateTable a) = ()
  parse OutputNada             = Right ()
  parse output                 = Left $ ExpectedNada output

instance ParseOutput (Delete D.One Nothing a) where
  type OutputT (Delete D.One Nothing a) = ()
  parse (OutputAffected 0)              = Right ()
  parse (OutputAffected 1)              = Right ()
  parse output                          = Left $ ExpectedMaybeOneAffected output

instance forall fs a. FromSqlValues fs => ParseOutput (Delete D.One (Just fs) a) where
  type OutputT (Delete D.One (Just fs) a) = Maybe fs
  parse (OutputRows [])                   = Right Nothing
  parse (OutputRows [row])                = Right $ Just $ fromSqlValues row
  parse output                            = Left  $ ExpectedMaybeOne output

instance ParseOutput (Delete D.Many Nothing a) where
  type OutputT (Delete D.Many Nothing a) = ()
  parse (OutputAffected _)               = Right ()
  parse output                           = Left $ ExpectedOneAffected output

instance forall fs a. FromSqlValues fs => ParseOutput (Delete D.Many (Just fs) a) where
  type OutputT (Delete D.Many (Just fs) a) = [fs]
  parse (OutputRows rows)                  = Right $ fromSqlValues <$> rows
  parse output                             = Left  $ ExpectedMaybeOne output

instance ParseOutput (Insert I.One a) where
  type OutputT (Insert I.One a) = ()
  parse (OutputAffected 1)      = Right ()
  parse output                  = Left $ ExpectedOneAffected output

instance ParseOutput (Insert I.Many a) where
  type OutputT (Insert I.Many a) = ()
  parse (OutputAffected _)       = Right ()
  parse output                   = Left $ ExpectedOneAffected output

instance forall fs a. FromSqlValues fs => ParseOutput (Select S.One fs a) where
  type OutputT (Select S.One fs a) = Maybe fs
  parse (OutputRows [])            = Right Nothing
  parse (OutputRows [row])         = Right $ Just $ fromSqlValues row
  parse output                     = Left  $ ExpectedMaybeOne output

instance forall fs a. FromSqlValues fs => ParseOutput (Select S.Many fs a) where
  type OutputT (Select S.Many fs a) = [fs]
  parse (OutputRows rows)           = Right $ fromSqlValues <$> rows
  parse output                      = Left  $ ExpectedMaybeOne output

-- TODO this needs to be made smarter. Requires knowledge of SQL behaviour.
instance ParseOutput (Head s) => ParseOutput (s :: [StatementType]) where
  type OutputT s = OutputT (Head s)
  parse = parse @(Head s)
