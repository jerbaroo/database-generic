module Database.Generic.Class where

import Database.Generic.Prelude
import Database.Generic.Statement (Statements)

-- | Monads that can communicate with a database over a given connection.
class (Exception (Error m t), Functor t, Monad m) => MonadDb m t c | m -> c where
  type Error m t :: Type
  execute :: c -> t Statements -> m (t (Either (Error m t) ()))

  -- delete :: forall a f b. (Entity f a, HasField f a b) => t b  -> m (Either (Error m t)        (t ()))
  -- select :: forall a f b. (Entity f a, HasField f a b) => t b  -> m (Either (Error m t) (Maybe (t a)))
  -- upsert :: forall a f  .  Entity f a                  => t a  -> m (Either (Error m t)        (t ()))

-- | Monads that can provide a dedicated NEW connection.
class MonadDbNewConn m c where
  newConn :: m c

-- | Monads that can provide temporary dedicated access to a connection.
class MonadDbWithConn m c where
  withConn :: (c -> m a) -> m a

-- * Internal

-- | Monads that maintain dedicated access to a connection.
--
-- DON'T create instances of this typeclass unless you know what you're doing!
class MonadDbHasConn m c where
  askConn :: m c
