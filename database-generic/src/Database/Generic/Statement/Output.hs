{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Generic.Statement.Output where

import Data.Aeson qualified as Aeson
import Database.Generic.Prelude
import Database.Generic.Entity.SqlTypes (SqlValue(..))
import Database.Generic.Entity.FromSql (FromSqlValues(..))
import Database.Generic.Statement.Type (StatementType(..))
import Database.Generic.Statement.Type.OneOrMany (OneOrMany(..))
import GHC.Generics (Generic)

-- | Output from executing an SQL statement.
data Output
  = OutputAffected !Integer
  | OutputNada
  | OutputRows ![[SqlValue]]
  deriving (Generic, Show)

instance Aeson.FromJSON Output
instance Aeson.ToJSON   Output

-- | The different types of output from executing SQL statements.
--
-- These constructors correspond to the constructors of 'Output'.
data OutputType = OutputTypeAffected | OutputTypeNada | OutputTypeRows
  deriving (Aeson.FromJSON, Generic, Show)

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

instance HasOutputType (Insert o Nothing a) where
  outputType = OutputTypeAffected

instance HasOutputType (Insert o (Just fs) a) where
  outputType = OutputTypeRows

instance HasOutputType (Select o fs a ob) where
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
  | ExpectedOne              !Output
  | ExpectedOneAffected      !Output
  | ExpectedOutputAffected   !Output
  | ExpectedOutputRows       !Output
  deriving Show

instance Exception OutputError where
  displayException (ExpectedMaybeOne         o) = "Expected 0 or 1 rows but got " <> show o
  displayException (ExpectedMaybeOneAffected o) = "Expected 0 or 1 rows affected but got " <> show o
  displayException (ExpectedNada             o) = "Expected OutputNada but got " <> show o
  displayException (ExpectedOne              o) = "Expected 1 row but got " <> show o
  displayException (ExpectedOneAffected      o) = "Expected 1 row affected but got " <> show o
  displayException (ExpectedOutputAffected   o) = "Expected OutputAffected but got " <> show o
  displayException (ExpectedOutputRows       o) = "Expected OutputRows but got " <> show o

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

instance ParseOutput (Delete One Nothing a) where
  type OutputT (Delete One Nothing a) = ()
  parse (OutputAffected 0)            = Right ()
  parse (OutputAffected 1)            = Right ()
  parse output                        = Left $ ExpectedMaybeOneAffected output

instance forall fs a. FromSqlValues fs => ParseOutput (Delete One (Just fs) a) where
  type OutputT (Delete One (Just fs) a) = Maybe fs
  parse (OutputRows [])                 = Right Nothing
  parse (OutputRows [row])              = Right $ Just $ fromSqlValues row
  parse output                          = Left  $ ExpectedMaybeOne output

instance ParseOutput (Delete Many Nothing a) where
  type OutputT (Delete Many Nothing a) = ()
  parse (OutputAffected _)             = Right ()
  parse output                         = Left $ ExpectedOneAffected output

instance forall fs a. FromSqlValues fs => ParseOutput (Delete Many (Just fs) a) where
  type OutputT (Delete Many (Just fs) a) = [fs]
  parse (OutputRows rows)                = Right $ fromSqlValues <$> rows
  parse output                           = Left  $ ExpectedMaybeOne output

instance ParseOutput (Insert One Nothing a) where
  type OutputT (Insert One Nothing a) = ()
  parse (OutputAffected 1)            = Right ()
  parse output                        = Left $ ExpectedOneAffected output

instance forall fs a. FromSqlValues fs => ParseOutput (Insert One (Just fs) a) where
  type OutputT (Insert One (Just fs) a) = Maybe fs
  parse (OutputRows [row])              = Right $ Just $ fromSqlValues row
  parse output                          = Left  $ ExpectedOne output

instance ParseOutput (Insert Many Nothing a) where
  type OutputT (Insert Many Nothing a) = ()
  parse (OutputAffected _)     = Right ()
  parse output                 = Left $ ExpectedOutputAffected output

instance forall fs a. FromSqlValues fs => ParseOutput (Insert Many (Just fs) a) where
  type OutputT (Insert Many (Just fs) a) = [fs]
  parse (OutputRows rows)                = Right $ fromSqlValues <$> rows
  parse output                           = Left  $ ExpectedOutputRows output

instance forall fs a ob. FromSqlValues fs => ParseOutput (Select One fs a ob) where
  type OutputT (Select One fs a ob) = Maybe fs
  parse (OutputRows [])             = Right Nothing
  parse (OutputRows [row])          = Right $ Just $ fromSqlValues row
  parse output                      = Left  $ ExpectedMaybeOne output

instance forall fs a ob. FromSqlValues fs => ParseOutput (Select Many fs a ob) where
  type OutputT (Select Many fs a ob) = [fs]
  parse (OutputRows rows)            = Right $ fromSqlValues <$> rows
  parse output                       = Left  $ ExpectedMaybeOne output

-- TODO this needs to be made smarter. Requires knowledge of SQL behaviour.
instance ParseOutput (Head s) => ParseOutput (s :: [StatementType]) where
  type OutputT s = OutputT (Head s)
  parse = parse @(Head s)
