module Database.Generic.Statement.Type where

import Database.Generic.Prelude
import Database.Generic.Statement.Type.OneOrMany (OneOrMany)

-- | All the type information we have about a statement.
data StatementType where
  BeginTx     ::                                          StatementType
  CommitTx    ::                                          StatementType
  CreateTable ::                          Type         -> StatementType
  Delete      :: OneOrMany -> Maybe fs -> Type         -> StatementType
  Insert      :: OneOrMany -> Maybe fs -> Type         -> StatementType
  Select      :: OneOrMany -> Type     -> Type -> Bool -> StatementType

data List a = One a | L a (List a)

type Head :: forall a. List a -> a
type family Head xs where
  Head (One a) = a
  Head (L a as) = Head as
