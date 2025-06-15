{-# LANGUAGE UndecidableInstances #-}

module Database.Generic.Statement.Output where

import Data.Aeson qualified as Aeson
import Database.Generic.Prelude
import Database.Generic.Entity.FromDb (FromDbValues(..))
import Database.Generic.Statement.Type (StatementType(..))
import Database.Generic.Statement.Type.OneOrMany (OneOrMany(..))

-- | Output from executing an SQL statement.
data Output dbv
  = OutputAffected !Integer
  | OutputNada
  | OutputRows ![[dbv]]
  deriving (Generic, Show)

instance Aeson.FromJSON dbv => Aeson.FromJSON (Output dbv)
instance Aeson.ToJSON   dbv => Aeson.ToJSON   (Output dbv)

-- | The different types of output from executing SQL statements.
--
-- These constructors correspond to the constructors of 'Output'.
data OutputType = OutputTypeAffected | OutputTypeNada | OutputTypeRows
  deriving (Generic, Show)

instance Aeson.FromJSON OutputType

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

data OutputError dbv
  = ExpectedMaybeOne         !(Output dbv)
  | ExpectedMaybeOneAffected !(Output dbv)
  | ExpectedNada             !(Output dbv)
  | ExpectedOne              !(Output dbv)
  | ExpectedOneAffected      !(Output dbv)
  | ExpectedOutputAffected   !(Output dbv)
  | ExpectedOutputRows       !(Output dbv)
  deriving Show

instance (Show dbv, Typeable dbv) => Exception (OutputError dbv) where
  displayException (ExpectedMaybeOne         o) = "Expected 0 or 1 rows but got " <> show o
  displayException (ExpectedMaybeOneAffected o) = "Expected 0 or 1 rows affected but got " <> show o
  displayException (ExpectedNada             o) = "Expected OutputNada but got " <> show o
  displayException (ExpectedOne              o) = "Expected 1 row but got " <> show o
  displayException (ExpectedOneAffected      o) = "Expected 1 row affected but got " <> show o
  displayException (ExpectedOutputAffected   o) = "Expected OutputAffected but got " <> show o
  displayException (ExpectedOutputRows       o) = "Expected OutputRows but got " <> show o

-- | Parse 'Output' into the return value expected from executing a statement.
class ParseOutput dbv s where
  type OutputT s
  parse :: Output dbv -> Either (OutputError dbv) (OutputT s)

instance ParseOutput dbv BeginTx where
  type OutputT BeginTx = ()
  parse OutputNada     = Right ()
  parse output         = Left $ ExpectedNada output

instance ParseOutput dbv CommitTx where
  type OutputT CommitTx = ()
  parse OutputNada      = Right ()
  parse output          = Left $ ExpectedNada output

instance ParseOutput dbv (CreateTable a) where
  type OutputT (CreateTable a) = ()
  parse OutputNada             = Right ()
  parse output                 = Left $ ExpectedNada output

instance ParseOutput dbv (Delete One Nothing a) where
  type OutputT (Delete One Nothing a) = ()
  parse (OutputAffected 0)            = Right ()
  parse (OutputAffected 1)            = Right ()
  parse output                        = Left $ ExpectedMaybeOneAffected output

instance forall fs a dbv. FromDbValues dbv fs => ParseOutput dbv (Delete One (Just fs) a) where
  type OutputT (Delete One (Just fs) a) = Maybe fs
  parse (OutputRows [])                 = Right Nothing
  parse (OutputRows [row])              = Right $ Just $ fromDbValues row
  parse output                          = Left  $ ExpectedMaybeOne output

instance ParseOutput dbv (Delete Many Nothing a) where
  type OutputT (Delete Many Nothing a) = ()
  parse (OutputAffected _)             = Right ()
  parse output                         = Left $ ExpectedOneAffected output

instance forall fs a dbv. FromDbValues dbv fs => ParseOutput dbv (Delete Many (Just fs) a) where
  type OutputT (Delete Many (Just fs) a) = [fs]
  parse (OutputRows rows)                = Right $ fromDbValues <$> rows
  parse output                           = Left  $ ExpectedMaybeOne output

instance ParseOutput dbv (Insert One Nothing a) where
  type OutputT (Insert One Nothing a) = ()
  parse (OutputAffected 1)            = Right ()
  parse output                        = Left $ ExpectedOneAffected output

instance forall fs a dbv. FromDbValues dbv fs => ParseOutput dbv (Insert One (Just fs) a) where
  type OutputT (Insert One (Just fs) a) = Maybe fs
  parse (OutputRows [row])              = Right $ Just $ fromDbValues row
  parse output                          = Left  $ ExpectedOne output

instance ParseOutput dbv (Insert Many Nothing a) where
  type OutputT (Insert Many Nothing a) = ()
  parse (OutputAffected _)     = Right ()
  parse output                 = Left $ ExpectedOutputAffected output

instance forall fs a dbv. FromDbValues dbv fs => ParseOutput dbv (Insert Many (Just fs) a) where
  type OutputT (Insert Many (Just fs) a) = [fs]
  parse (OutputRows rows)                = Right $ fromDbValues <$> rows
  parse output                           = Left  $ ExpectedOutputRows output

instance forall fs a ob dbv. FromDbValues dbv fs => ParseOutput dbv (Select One fs a ob) where
  type OutputT (Select One fs a ob) = Maybe fs
  parse (OutputRows [])             = Right Nothing
  parse (OutputRows [row])          = Right $ Just $ fromDbValues row
  parse output                      = Left  $ ExpectedMaybeOne output

instance forall fs a ob dbv. FromDbValues dbv fs => ParseOutput dbv (Select Many fs a ob) where
  type OutputT (Select Many fs a ob) = [fs]
  parse (OutputRows rows)            = Right $ fromDbValues <$> rows
  parse output                       = Left  $ ExpectedMaybeOne output

-- TODO this needs to be made smarter. Requires knowledge of SQL behaviour.
instance ParseOutput dbv (Head s) => ParseOutput dbv (s :: [StatementType]) where
  type OutputT s = OutputT (Head s)
  parse = parse @dbv @(Head s)
