module Database.Generic.Statement.Type where

import Database.Generic.Prelude
import Database.Generic.Statement.Type.OneOrMany (OneOrMany(..))

-- | All the type information we have about a statement.
data StatementType where
  BeginTx     :: StatementType
  CommitTx    :: StatementType
  CreateTable :: Type -> StatementType
  Delete      :: OneOrMany -> Maybe x -> Type -> StatementType
  Insert      :: OneOrMany -> Type    ->         StatementType
  Select      :: OneOrMany -> Type    -> Type -> StatementType

-- | Add a type to the front of a list of types.
type Cons :: forall a. a -> [a] -> [a]
type family Cons xs a where
  Cons a '[]    = '[a]
  Cons a (x:xs) = a:xs
