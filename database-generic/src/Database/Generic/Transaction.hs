module Database.Generic.Transaction where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader(ask), ReaderT(ReaderT), runReaderT)
import Database.Generic.Class (MonadDb(..), MonadDbHasConn(..), ExecuteError)
import Database.Generic.Prelude
import Unsafe.Coerce (unsafeCoerce)

-- TODO docs
newtype Tx m c a = Tx (ReaderT c m a)
  deriving newtype (Applicative, Functor, Monad, MonadIO, MonadReader c)

instance Monad m => MonadDbHasConn (Tx m c) c where
  askDbConn = ask

instance MonadDb m t c => MonadDb (Tx m c) t c where
  type Error (Tx m c) t = Error m t
  executeStatement c t = do
    -- 'unsafeCoerce' is safe here because the error types are equal (2 lines up).
    let mapError :: ExecuteError (Error m t) -> ExecuteError (Error (Tx m c) t)
        mapError = unsafeCoerce
    Tx $ ReaderT $ const $
      fmap (mapLeft mapError) <$> executeStatement @m @t c t

runTx :: c -> Tx m c a -> m a
runTx c (Tx m) = runReaderT m c
