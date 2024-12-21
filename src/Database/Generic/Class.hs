module Database.Generic.Class where

import Data.Kind (Type)
import Database.Generic.Entity (Entity)
import Database.Generic.Prelude

-- | Monads that can communicate with a database.
class Show (Error m t) => MonadDb m t where
  type Error m t :: Type
  createTableIfNotExists :: forall a f. Entity f a => m (Either (Error m t) (t ()))
  delete :: forall a f b. (Entity f a, HasField f a b) => t b -> m (Either (Error m t)        (t ()))
  select :: forall a f b. (Entity f a, HasField f a b) => t b -> m (Either (Error m t) (Maybe (t a)))
  upsert :: forall a f  .  Entity f a                  => t a -> m (Either (Error m t)        (t ()))
