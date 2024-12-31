module Database.Generic.Operations where

import Database.Generic.Class (MonadDb(..), MonadDbHasConn(..), MonadDbNewConn(..), MonadDbWithConn(..))
import Database.Generic.Class qualified as Db
import Database.Generic.Prelude
import Database.Generic.Statement (Statements)
import Database.Generic.Statement qualified as Statement
import Database.Generic.Transaction (Tx, runTx)

-- | Execute 'Statements' via the current database connection 'c'.
execute :: forall m c r.
  (MonadDb m Identity c, MonadDbHasConn m c) =>
  Statements r -> m (Either (Error m Identity) ())
execute = fmap extract . (askConn >>=) . flip (Db.execute @_ @Identity) . pure

-- | Like 'execute' but each 'Statements' is appended with a commit statement.
executeTx :: forall m c r.
  (MonadDb m Identity c, MonadDbWithConn m c) =>
  Statements r -> m (Either (Error m Identity) ())
executeTx = fmap extract . withConn .
  flip (Db.execute @_ @Identity) . pure . Statement.commitTx

-- | Like 'executeTx' but shape of input and output is of type 't'.
executeTxs :: forall m t c r.
  (MonadDb m t c, MonadDbNewConn m c) =>
  t (Statements r) -> m (t (Either (Error m t) ()))
executeTxs = (newConn >>=) . flip Db.execute . fmap Statement.commitTx

-- | Run the provided actions, then run a commit statement.
tx :: forall m c a.
  (MonadDb m Identity c, MonadDbWithConn m c) =>
  Tx m c (Either (Error m Identity) a) -> m (Either (Error m Identity) a)
tx m = withConn \c ->
  runTx c $ m >>= \case
    Left  e1 -> pure $ Left e1
    Right a  -> do
      let commit = Statement.statements Statement.StatementCommitTx
      Database.Generic.Operations.execute commit >>= \case
        Left  e2 -> pure $ Left e2
        Right () -> pure $ Right a
