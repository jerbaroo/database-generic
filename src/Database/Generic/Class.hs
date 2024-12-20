module Database.Generic.Class where

import Database.Generic.Entity (Entity)
import Data.Kind (Type)

-- | Monads that can communicate with a database.
class MonadDB m f where
  type Error m f :: Type
  select         :: Entity a b => f b -> m (Either (Error m g) (Maybe (f a)))
