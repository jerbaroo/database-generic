module Database.Generic.Class where

import Data.Kind (Type)
import Database.Generic.Entity (Entity)
import Database.Generic.Prelude
import Database.Generic.Statement qualified as Statement

-- | Monads that can communicate with a database.
-- TODO must be a generic interface to allow network proxying.
class Show (Error m t) => MonadDb m t where
  type Error m t :: Type
  createTable :: t Statement.CreateTable -> m (Either (Error m t) (t ()))
  delete :: forall a f b. (Entity f a, HasField f a b) => t b  -> m (Either (Error m t)        (t ()))
  select :: forall a f b. (Entity f a, HasField f a b) => t b  -> m (Either (Error m t) (Maybe (t a)))
  upsert :: forall a f  .  Entity f a                  => t a  -> m (Either (Error m t)        (t ()))
