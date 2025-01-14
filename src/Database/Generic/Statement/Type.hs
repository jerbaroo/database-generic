module Database.Generic.Statement.Type where

import Database.Generic.Prelude
import Database.Generic.Statement.Delete qualified as D
import Database.Generic.Statement.Insert qualified as I
import Database.Generic.Statement.Select qualified as S

-- | All the type information we have about a statement.
data StatementType where
  BeginTx     :: StatementType
  CommitTx    :: StatementType
  CreateTable :: Type -> StatementType
  Delete      :: D.OneOrMany -> Type ->         StatementType
  Insert      :: I.OneOrMany -> Type ->         StatementType
  Select      :: S.OneOrMany -> Type -> Type -> StatementType

-- | Add a type to the front of a list of types.
type Cons :: forall a. a -> [a] -> [a]
type family Cons xs a where
  Cons a '[]    = '[a]
  Cons a (x:xs) = a:xs
