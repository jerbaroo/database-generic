module Database.Generic.Class where

import Database.Generic.Prelude
import Database.Generic.Statement (Statement)
import Database.Generic.Statement.NoType qualified as NT
import Database.Generic.Statement.Output (HasOutputType, Output, OutputError, OutputType, outputType)

-- | Monads that can communicate with a database over a given connection.
class (Exception (Error m t), Functor t, Monad m) => MonadDb m t c | m -> c where
  type Error m t :: Type
  type Error m t = SomeException -- Default for convenience.

  -- | Information about the type of statement is thrown away at this point.
  executeStatement :: HasOutputType r =>
    c -> t (Statement r) -> m (t (Either (Error m t) Output))

  executeStatement'' ::
    c -> t (NT.Statement, OutputType) -> m (t (Either (Error m t) Output))

  -- | Lift an 'OutputError' into the error type for this instance.
  outputError :: OutputError -> Error m t
  default outputError :: (SomeException ~ Error m t) => OutputError -> Error m t
  outputError = toException

  -- | Information about the type of statement is thrown away at this point.
executeStatement' :: forall m t c r. (HasOutputType r, MonadDb m t c) =>
    c -> t (Statement r) -> m (t (Either (Error m t) Output))
executeStatement' c t =
  executeStatement'' c (t <&> \s -> (from s, outputType @r))

-- | Monads that can provide a dedicated NEW connection.
class MonadDbNewConn m c where
  newDbConn :: m c

-- | Monads that can provide temporary dedicated access to a connection.
class MonadDbWithConn m c where
  withDbConn :: (c -> m a) -> m a

instance {-# OVERLAPPABLE #-} (Monad m, MonadDbNewConn m c) => MonadDbWithConn m c where
  withDbConn = (newDbConn >>=)

-- * Internal

-- | Monads that maintain dedicated access to a connection.
--
-- DON'T create instances of this typeclass unless you know what you're doing!
class MonadDbHasConn m c where
  askDbConn :: m c
