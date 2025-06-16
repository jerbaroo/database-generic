module Database.Generic.Statement.Type where

import Database.Generic.Prelude
import Database.Generic.Statement.Type.OneOrMany (OneOrMany(..))

-- | All the type information we have about a statement.
data StatementType where
  BeginTx     ::                                          StatementType
  CommitTx    ::                                          StatementType
  CreateTable ::                          Type         -> StatementType
  Delete      :: OneOrMany -> Maybe fs -> Type         -> StatementType
  Insert      :: OneOrMany -> Maybe fs -> Type         -> StatementType
  Select      :: OneOrMany -> Type     -> Type -> Bool -> StatementType

-- | Add a type to the front of a list of types.
type Cons :: forall a. a -> [a] -> [a]
type family Cons xs a where
  Cons a '[] = '[a]
  Cons a xs  = a:xs

type TupleCons :: forall a b c. a -> b -> c
type family TupleCons a b where
  TupleCons a (b, c, d, e, f) = '(a, b, c, d, e, f)
  TupleCons a (b, c, d, e) = '(a, b, c, d, e)
  TupleCons a (b, c, d) = '(a, b, c, d)
  TupleCons a (b, c) = '(a, b, c)
  TupleCons a b = '(a, b)
