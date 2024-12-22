module Database.Generic.Class where

import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Kind (Type)
import Database.Generic.Entity (Entity)
import Database.Generic.Prelude
import Database.Generic.Statement qualified as Statement

-- | Monads that can communicate with a database over a given connection.
class Exception (Error m t c) => MonadDb m t c where
  type Error m t c :: Type
  createTable :: c -> t Statement.CreateTable -> m (Either (Error m t c) (t ()))

  -- TODO remove.
  delete :: forall a f b. (Entity f a, HasField f a b) => t b  -> m (Either (Error m t c)        (t ()))
  select :: forall a f b. (Entity f a, HasField f a b) => t b  -> m (Either (Error m t c) (Maybe (t a)))
  upsert :: forall a f  .  Entity f a                  => t a  -> m (Either (Error m t c)        (t ()))

-- | Monads that can manage database connections.
class MonadDbConn m c where
  connect :: m c

-- | Monads that have a single currently active connection.
class MonadDbActiveConn m c where
  activeConn :: m c

-- | Monad that has a single currently active connection 'c'.
newtype MonadTx m c a = MonadTx (ReaderT c m a)
  deriving newtype (Applicative, Functor, Monad, MonadIO, MonadReader c)

instance Monad m => MonadDbActiveConn (MonadTx m c) c where
  activeConn = ask

runMonadTx :: c -> MonadTx m c a -> m a
runMonadTx c (MonadTx m) = runReaderT m c

withConn :: forall m t c a. (Monad m, MonadDb m t c, MonadDbConn m c) => (MonadTx m c a) -> m a
withConn f = do
  c <- (connect @m @c)
  runMonadTx c f
