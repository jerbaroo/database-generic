{-# LANGUAGE UndecidableInstances #-}

module Database.Generic.Class where

import Database.Generic.Prelude
import Database.Generic.Statement (Statement)
import Database.Generic.Statement.NoType qualified as NT
import Database.Generic.Statement.Output (HasOutputType, Output, OutputError, OutputType, outputType)

-- | Monads that can communicate with a database over a given connection.
class (Exception (Error m t), Functor t, Monad m, Show (Error m t))
  => MonadDb m t c dbv | m -> c, m -> dbv where

  type Error m t :: Type
  type Error m t = SomeException -- Default for convenience.

  -- | Execute a statement and parse the output based on expected 'OutputType'.
  executeStatement
    :: c
    -> t (NT.Statement, OutputType)
    -> m (t (Either (ExecuteError (Error m t) dbv) (Output dbv)))

-- | Error on execution of a statement.
data ExecuteError a dbv
  = ExeCustomError !a
  | ExeOutputError !(OutputError dbv)
  deriving Show

instance (Exception a, Show dbv, Typeable dbv) => Exception (ExecuteError a dbv)

instance From (OutputError dbv) (ExecuteError a dbv) where
  from = ExeOutputError

-- | Like 'executeStatement' but takes a 'Statement' that still has type info 'r'.
executeStatement' :: forall m t c r dbv. (HasOutputType r, MonadDb m t c dbv) =>
  c -> t (Statement r) -> m (t (Either (ExecuteError (Error m t) dbv) (Output dbv)))
executeStatement' c = executeStatement c . fmap \s -> (from s, outputType @r)

-- | Monads that can provide a dedicated NEW connection.
class MonadDbNewConn m c where
  newDbConn :: m c

-- | Monads that can provide temporary dedicated access to a connection.
class MonadDbWithConn m c where
  withDbConn :: (c -> m a) -> m a

-- | By default a new connection is created everytime 'withDbConn' is called.
--
-- You might decide to override this implementation to use a connection pool.
instance {-# OVERLAPPABLE #-} (Monad m, MonadDbNewConn m c)
  => MonadDbWithConn m c where

  withDbConn = (newDbConn >>=)

-- * Internal

-- | Monads that maintain dedicated access to a connection.
--
-- DON'T create instances of this typeclass unless you know what you're doing!
class MonadDbHasConn m c where
  askDbConn :: m c
