
module Database.Generic.Operations where

import Database.Generic.Class (MonadDb(..), MonadDbHasConn(..), MonadDbNewConn(..), MonadDbWithConn(..))
import Database.Generic.Class qualified as Db
import Database.Generic.Statement.Output (HasOutputType, OutputT, ParseOutput(..))
import Database.Generic.Prelude
import Database.Generic.Statement (Statement(..), CommitType)
import Database.Generic.Statement qualified as Statement
import Database.Generic.Statement.Returning (StatementType(..))
import Database.Generic.Statement.Tx qualified as Tx
import Database.Generic.Transaction (Tx, runTx)

-- | Execute 'Statement' via the current database connection 'c'.
execute :: forall m c r.
  (HasOutputType r, MonadDb m Identity c, MonadDbHasConn m c, ParseOutput r) =>
  Statement r -> m (Either (Error m Identity) (OutputT r))
execute =
  fmap extract . (askConn >>=) . flip (executeAndParse @_ @Identity) . pure

-- | Like 'execute' but each 'Statement' is appended with a commit statement.
executeTx :: forall m c r.
  (HasOutputType (CommitType r), MonadDb m Identity c, MonadDbWithConn m c, ParseOutput (CommitType r)) =>
  Statement r -> m (Either (Error m Identity) (OutputT (CommitType r)))
executeTx = fmap extract . withConn .
  flip (executeAndParse @_ @Identity) . pure . Statement.commitTx

-- | Like 'executeTx' but shape of input and output is of type 't'.
executeTxs :: forall m t c r.
  (HasOutputType (CommitType r), MonadDb m t c, MonadDbNewConn m c, ParseOutput (CommitType r)) =>
  t (Statement r) -> m (t (Either (Error m t) (OutputT (CommitType r))))
executeTxs = (newConn >>=) . flip executeAndParse . fmap Statement.commitTx

-- | Run the provided actions, then run a commit statement.
tx :: forall m c a.
  (MonadDb m Identity c, MonadDbWithConn m c) =>
  Tx m c (Either (Error m Identity) a) -> m (Either (Error m Identity) a)
tx m = withConn \c -> runTx c $ m >>= \case
  Left  e1 -> pure $ Left e1
  Right a  ->
    Database.Generic.Operations.execute (StatementCommitTx Tx.CommitTx) >>= \case
      Left  e2 -> pure $ Left e2
      Right () -> pure $ Right a

-- * Internal.

-- | Slim wrapper over 'Db.execute' which also parses output.
executeAndParse
  :: forall m t c r. (HasOutputType r, MonadDb m t c, ParseOutput r)
  => c
  -> t (Statement (r :: StatementType))
  -> m (t (Either (Db.Error m t) (OutputT r)))
executeAndParse c ts = fmap f <$> Db.execute @_ @t c ts
 where
  f (Left  l) = Left l
  f (Right o) = mapLeft (Db.outputError @m @t) $ parse @r o
