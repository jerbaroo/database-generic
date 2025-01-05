{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Parsing output.
module Database.Generic.Output where

import Database.Generic.Prelude
import Database.Generic.Entity.SqlTypes (SqlValue(..))
import Database.Generic.Entity.FromSql (FromSqlValues(..))

-- | Types of values returned from SQL statements.
data Returning a where
  MaybeOne    :: Type -> Returning a
  Nada        ::         Returning a
  OneAffected :: Type -> Returning a

-- * Output.

-- | Output of SQL statement before parsing into return value.
data Output
  = OutputAffected !Integer
  | OutputRows     ![[SqlValue]]
  | OutputNada
  deriving Show

-- | Types of output values from SQL statements.
data OutputType = OutputTypeAffected | OutputTypeRows | OutputTypeNada

class HasOutputType r where
  outputType :: OutputType

instance HasOutputType (MaybeOne a) where
  outputType = OutputTypeRows

instance HasOutputType Nada where
  outputType = OutputTypeNada

instance HasOutputType (OneAffected a) where
  outputType = OutputTypeAffected

-- * Parsing output.

data OutputError
  = ExpectedMaybeOne    !Output
  | ExpectedNada        !Output
  | ExpectedOneAffected !Output
  deriving Show

instance Exception OutputError where
  displayException (ExpectedMaybeOne    o) = "Expected 0 or 1 rows but got " <> show o
  displayException (ExpectedNada        o) = "Expected OutputNada but got " <> show o
  displayException (ExpectedOneAffected o) = "Expected OutputAffected but got " <> show o

class ParseOutput r where
  type OutputT r
  parse :: Output -> Either OutputError (OutputT r)

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
