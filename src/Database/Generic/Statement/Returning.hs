module Database.Generic.Statement.Returning where

import Database.Generic.Prelude

-- | Type of one SQL statement.
data StatementType where
  BeginTx      ::         StatementType
  CommitTx     ::         StatementType
  ManyAffected :: Type -> StatementType
  MaybeOne     :: Type -> StatementType
  Nada         ::         StatementType -- TODO replace with CreateTable.
  OneAffected  :: Type -> StatementType

-- | Type of values returned by a statement of type 'StatementType'.
type        ReturnType :: forall a. StatementType -> a
type family ReturnType s where
  ReturnType (MaybeOne a) = a

-- | Adapt a 'StatementType' so the statement returns values of type 'a'.
type        Returning :: forall a. StatementType -> a -> StatementType
type family Returning s a where
  Returning (MaybeOne _) a = MaybeOne a

-- | Add a type to the front of a list of types.
type Cons :: forall a. a -> [a] -> [a]
type family Cons xs a where
  Cons a '[]    = '[a]
  Cons a (x:xs) = a:xs
