{-# LANGUAGE UndecidableInstances #-}

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

instance MonadDb m db => MonadDb (Tx m c) db where
  type C     (Tx m c) db = C     m db
  type T     (Tx m c) db = T     m db
  type Error (Tx m c) db = Error m db
  executeStatement c t = do
    let mapError
          :: ExecuteError (Error m        db) dbv
          -> ExecuteError (Error (Tx m c) db) dbv
        mapError = unsafeCoerce -- safe because the error types are equal.
    Tx $ ReaderT $ const $
      fmap (mapLeft mapError) <$> executeStatement c t

runTx :: c -> Tx m c a -> m a
runTx c (Tx m) = runReaderT m c
