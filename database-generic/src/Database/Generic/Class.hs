{-# LANGUAGE UndecidableInstances #-}

module Database.Generic.Class where

import Database.Generic.Database (Database, DbV)
import Database.Generic.Prelude
import Database.Generic.Statement (Statement)
import Database.Generic.Statement.NoType qualified as NT
import Database.Generic.Statement.Output (HasOutputType, Output, OutputError, OutputType, outputType)

type ExecuteReturns m db a = Either (ExecuteError (Error m db) (DbV db)) a

-- | Monads that can execute database statements.
class
  ( Applicative (T m db)
  , Comonad (T m db)
  , Database db
  , Exception (Error m db)
  , Monad m
  , Show (Error m db)
  )
  => MonadDb m db | m -> db where

  type C m db :: Type

  type T m db :: Type -> Type
  type T m db = Identity -- Default for convenience.

  type Error m db :: Type
  type Error m db = SomeException -- Default for convenience.

  -- | Execute a statement and parse the output based on expected 'OutputType'.
  executeStatement
    :: C m db
    -> (T m db) (NT.Statement, OutputType)
    -> m ((T m db) (ExecuteReturns m db (Output (DbV db))))

-- | Error on execution of a statement.
data ExecuteError a dbv
  = ExeCustomError !a
  | ExeOutputError !(OutputError dbv)
  deriving Show

instance (Exception a, Show dbv, Typeable dbv) => Exception (ExecuteError a dbv)

instance From (OutputError dbv) (ExecuteError a dbv) where
  from = ExeOutputError

-- | Like 'executeStatement' but takes a 'Statement' that still has type info 'r'.
executeStatement' :: forall m r db.
  (Database db, Functor (T m db), HasOutputType r, MonadDb m db)
  => C m db
  -> (T m db) (Statement r)
  -> m ((T m db) (ExecuteReturns m db (Output (DbV db))))
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
