module Database.Generic.Class where

import Control.Monad.Reader (MonadReader, ReaderT(..), ask, runReaderT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Kind (Type)
import Database.Generic.Entity (Entity)
import Database.Generic.Prelude
import Database.Generic.Statement qualified as Statement

newtype Error = Error String

instance Exception Error

instance Show Error where
  show (Error s) = s

-- | Monads that can communicate with a database over a given connection.
class MonadDb m t c where
  createTable :: c -> t Statement.CreateTable -> m (Either Error (t ()))

  -- TODO remove.
  delete :: forall a f b. (Entity f a, HasField f a b) => t b  -> m (Either Error        (t ()))
  select :: forall a f b. (Entity f a, HasField f a b) => t b  -> m (Either Error (Maybe (t a)))
  upsert :: forall a f  .  Entity f a                  => t a  -> m (Either Error        (t ()))

-- | Monads that can provide isolated access to an open connection.
class MonadDbConn m c where
  withConn :: (c -> m a) -> m a

-- | Monads that have isolated access to an open connection.
class MonadDbActiveConn m c where
  activeConn :: m c

-- * Transactions.

newtype Tx m c a = Tx (ReaderT c m a)
  deriving newtype (Applicative, Functor, Monad, MonadIO, MonadReader c)

instance Monad m => MonadDbActiveConn (Tx m c) c where
  activeConn = ask

instance MonadDb m t c => MonadDb (Tx m c) t c where

runTx :: c -> Tx m c a -> m a
runTx c (Tx m) = runReaderT m c

-- | Run the provided database actions between calls to 'begin' and 'commit'.
tx :: forall m t c a. (Monad m, MonadDb m t c, MonadDbConn m c) => Tx m c a -> m a
tx m = withConn \c -> do
  runTx c m
