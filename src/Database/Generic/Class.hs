module Database.Generic.Class where

import Database.Generic.Prelude
import Database.Generic.Statement (Statements)

-- | Monads that can communicate with a database over a given connection.
class (Exception (Error m t), Functor t, Monad m) => MonadDb m t c | m -> c where
  type Error m t :: Type
  type Error m t = SomeException -- Default for convenience.

  -- | Information about the type of statement is thrown away at this point.
  execute :: c -> t (Statements r) -> m (t (Either (Error m t) ()))

-- | Monads that can provide a dedicated NEW connection.
class MonadDbNewConn m c where
  newConn :: m c

-- | Monads that can provide temporary dedicated access to a connection.
class MonadDbWithConn m c where
  withConn :: (c -> m a) -> m a

instance {-# OVERLAPPABLE #-} (Monad m, MonadDbNewConn m c) => MonadDbWithConn m c where
  withConn = (newConn >>=)

-- * Internal

-- | Monads that maintain dedicated access to a connection.
--
-- DON'T create instances of this typeclass unless you know what you're doing!
class MonadDbHasConn m c where
  askConn :: m c
