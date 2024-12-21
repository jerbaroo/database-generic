module Database.Generic.Class where

import Data.Kind (Type)
import Database.Generic.Entity (Entity)
import Database.Generic.Prelude

-- | Monads that can communicate with a database.
class Show (Error m t) => MonadDb m t where
  type Error m t :: Type
  delete :: forall a f b. (Entity a f, HasField f a b) => t b -> m (Either (Error m t)        (t ()))
  select :: forall a f b. (Entity a f, HasField f a b) => t b -> m (Either (Error m t) (Maybe (t a)))
  upsert :: forall a f  .  Entity a f                  => t a -> m (Either (Error m t)        (t ()))
