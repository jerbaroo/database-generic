module Database.Generic.Transaction where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader(ask), ReaderT(ReaderT), runReaderT)
import Database.Generic.Class (MonadDb(..), MonadDbHasConn(..))
import Database.Generic.Prelude

newtype Tx m c a = Tx (ReaderT c m a)
  deriving newtype (Applicative, Functor, Monad, MonadIO, MonadReader c)

instance Monad m => MonadDbHasConn (Tx m c) c where
  askConn = ask

instance MonadDb m t c => MonadDb (Tx m c) t c where
  type Error (Tx m c) t = Error m t
  execute c s = Tx $ ReaderT $ const $ execute @m @t c s

runTx :: c -> Tx m c a -> m a
runTx c (Tx m) = runReaderT m c
