module Database.Generic.Operations where

import Database.Generic.Class (MonadDb(..), MonadDbHasConn(..), MonadDbNewConn(..), MonadDbWithConn(..))
import Database.Generic.Class qualified as Db
import Database.Generic.Prelude
import Database.Generic.Statement (Statements)
import Database.Generic.Statement qualified as Statement
import Database.Generic.Transaction (Tx, runTx)

-- | Execute 'Statements' within a transaction.
execute :: forall m c.
  (MonadDb m Identity c, MonadDbHasConn m c) =>
  Statements -> m (Either (Error m Identity) ())
execute = fmap extract . (askConn >>=) . flip (Db.execute @_ @Identity) . pure

-- | Execute ('Statements' appended with "COMMIT TRANSACTION") atomically.
executeTx :: forall m c.
  (MonadDb m Identity c, MonadDbWithConn m c) =>
  Statements -> m (Either (Error m Identity) ())
executeTx = fmap extract . withConn .
  flip (Db.execute @_ @Identity) . pure . (<> Statement.commitTx)

-- | Execute each ('Statements' appended with "COMMIT TRANSACTION") atomically.
executeTxs :: forall m t c.
  (MonadDb m t c, MonadDbNewConn m c) =>
  t Statements -> m (t (Either (Error m t) ()))
executeTxs = (newConn >>=) . flip Db.execute . fmap (<> Statement.commitTx)

-- | Run the provided actions, finally followed by a "COMMIT TRANSACTION".
tx :: forall m c a.
  (MonadDb m Identity c, MonadDbWithConn m c) =>
  Tx m c (Either (Error m Identity) a) -> m (Either (Error m Identity) a)
tx m = withConn \c -> runTx c $ m >>= \case
  Left  e1 -> pure $ Left e1
  Right a  -> Database.Generic.Operations.execute Statement.commitTx >>= \case
    Left  e2 -> pure $ Left e2
    Right () -> pure $ Right a
