
module Database.Generic.Statement.Output where

import Database.Generic.Prelude
import Database.Generic.Entity.SqlTypes (SqlValue(..))
import Database.Generic.Entity.FromSql (FromSqlValues(..))
import Database.Generic.Statement.Returning (StatementType(..))

-- | Output of an SQL statement.
data Output
  = OutputAffected !Integer
  | OutputRows     ![[SqlValue]]
  | OutputNada
  deriving Show

-- | The different types of output from SQL statements.
--
-- These constructors correspond to the constructors of 'Output'.
data OutputType = OutputTypeAffected | OutputTypeRows | OutputTypeNada

-- | The type of output expected from an SQL statement of type 's'.
class HasOutputType s where
  outputType :: OutputType

instance HasOutputType (ManyAffected a) where
  outputType = OutputTypeAffected

instance HasOutputType (MaybeOne a) where
  outputType = OutputTypeRows

instance HasOutputType Nada where
  outputType = OutputTypeNada

instance HasOutputType (OneAffected a) where
  outputType = OutputTypeAffected

data OutputError
  = ExpectedMaybeOne       !Output
  | ExpectedNada           !Output
  | ExpectedOneAffected    !Output
  | ExpectedOutputAffected !Output
  deriving Show

instance Exception OutputError where
  displayException (ExpectedMaybeOne       o) = "Expected 0 or 1 rows but got " <> show o
  displayException (ExpectedNada           o) = "Expected OutputNada but got " <> show o
  displayException (ExpectedOneAffected    o) = "Expected one affected but got " <> show o
  displayException (ExpectedOutputAffected o) = "Expected OutputAffected but got " <> show o

-- | Parse 'Output' into the value expected from executing a statement.
class ParseOutput (s :: StatementType) where
  type OutputT s
  parse :: Output -> Either OutputError (OutputT s)

instance ParseOutput (ManyAffected a) where
  type OutputT (ManyAffected a) = ()
  parse (OutputAffected _)      = Right ()
  parse output                  = Left $ ExpectedOneAffected output

instance forall a. FromSqlValues a => ParseOutput (MaybeOne a) where
  type OutputT (MaybeOne a) = Maybe a
  parse (OutputRows [])     = Right Nothing
  parse (OutputRows [row])  = Right $ Just $ fromSqlValues row
  parse output              = Left  $ ExpectedMaybeOne output

instance ParseOutput Nada where
  type OutputT Nada = ()
  parse OutputNada  = Right ()
  parse output      = Left $ ExpectedNada output

instance ParseOutput (OneAffected a) where
  type OutputT (OneAffected a) = ()
  parse (OutputAffected 1)     = Right ()
  parse output                 = Left $ ExpectedOneAffected output
