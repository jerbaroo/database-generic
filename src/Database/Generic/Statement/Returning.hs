module Database.Generic.Statement.Returning where

import Database.Generic.Prelude

-- | Types of values returned from SQL statements.
data Returning where
  MaybeOne    :: Type -> Returning
  Nada        ::         Returning
  OneAffected :: Type -> Returning

type ReturningType :: forall r a. r -> a
type family ReturningType r where
  ReturningType (MaybeOne a) = a
