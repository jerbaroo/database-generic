module Database.Generic.Statement.Returning where

import Database.Generic.Prelude

-- | Types of values returned from SQL statements.
data Returning a where
  MaybeOne    :: Type -> Returning a
  Nada        ::         Returning a
  OneAffected :: Type -> Returning a

type ReturningType :: forall r a. r a -> a
type family ReturningType r where
  ReturningType (MaybeOne a) = a
