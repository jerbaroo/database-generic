module Database.Generic.Operations where

import Database.Generic.Class (MonadDb(..), MonadDbHasConn(..), MonadDbNewConn(..), MonadDbWithConn(..))
import Database.Generic.Class qualified as Db
import Database.Generic.Prelude
import Database.Generic.Statement (Statement(..), ToStatement(..))
import Database.Generic.Statement qualified as Statement
import Database.Generic.Statement.Output (HasOutputType, OutputT, ParseOutput(..))
import Database.Generic.Statement.Type (StatementType(..), Cons)
import Database.Generic.Statement.Tx qualified as Tx
import Database.Generic.Transaction (Tx, runTx)

-- | Convert 'r' to a 'Statement'and execute via current database connection.
execute :: forall m c r s.
  ( HasOutputType s
  , MonadDb m Identity c
  , MonadDbHasConn m c
  , ParseOutput s
  , ToStatement r
  , s ~ S r
  )
  => r
  -> m (Either (Error m Identity) (OutputT s))
execute =
  fmap extract . (askDbConn >>=)
  . flip (executeAndParse @_ @Identity) . pure . statement

-- | Like 'execute' but each 'Statement' is appended with a commit statement.
executeTx :: forall m c r s.
  ( HasOutputType (Cons CommitTx s)
  , MonadDb m Identity c
  , MonadDbWithConn m c
  , ParseOutput (Cons CommitTx s)
  , ToStatement r
  , s ~ S r
  )
  => r
  -> m (Either (Error m Identity) (OutputT (Cons CommitTx s)))
executeTx = fmap extract . withDbConn .
  flip (executeAndParse @_ @Identity) . pure . Statement.commitTx . statement

-- | Like 'executeTx' but shape of input and output is of type 't'.
executeTxs :: forall m t c r s.
  ( HasOutputType (Cons CommitTx s)
  , MonadDb m t c
  , MonadDbNewConn m c
  , ParseOutput (Cons CommitTx s)
  , ToStatement r
  , s ~ S r
  )
  => t r
  -> m (t (Either (Error m t) (OutputT (Cons CommitTx s))))
executeTxs =
  (newDbConn >>=) . flip executeAndParse . fmap (Statement.commitTx . statement)

-- | Run the provided actions, then run a commit statement.
tx :: forall m c a.
  (MonadDb m Identity c, MonadDbWithConn m c)
  => Tx m c (Either (Error m Identity) a)
  -> m (Either (Error m Identity) a)
tx m = withDbConn \c -> runTx c $ m >>= \case
  Left  e1 -> pure $ Left e1
  Right a  -> Database.Generic.Operations.execute Tx.CommitTx >>= \case
    Left  e2 -> pure $ Left e2
    Right () -> pure $ Right a

tx_ :: forall m c a.
  (MonadDb m Identity c, MonadDbWithConn m c)
  => Tx m c (Either (Error m Identity) a)
  -> m a
tx_ =
  fmap (fromEither \e -> error $ "Error in tx_: " <> displayException e) . tx

-- * Internal.

-- | Slim wrapper over 'Db.execute' which also parses output.
executeAndParse :: forall m t c s.
  (HasOutputType s, MonadDb m t c, ParseOutput s)
  => c
  -> t (Statement s)
  -> m (t (Either (Db.Error m t) (OutputT s)))
executeAndParse c ts = fmap f <$> Db.executeStatement' @_ @t c ts
 where
  f (Left  l) = Left l
  f (Right o) = mapLeft (Db.outputError @m @t) $ parse @s o
