-- TODO docs
module Database.Generic.Operations where

import Database.Generic.Class (Error, ExecuteError, MonadDb, MonadDbHasConn(..), MonadDbNewConn(..), MonadDbWithConn(..))
import Database.Generic.Class qualified as Db
import Database.Generic.Prelude
import Database.Generic.Statement (Statement(..), ToStatement(..))
import Database.Generic.Statement qualified as Statement
import Database.Generic.Statement.Output (HasOutputType, OutputT, ParseOutput(..))
import Database.Generic.Statement.Type (StatementType(..), Cons)
import Database.Generic.Statement.Tx qualified as Tx
import Database.Generic.Transaction (Tx, runTx)

-- | Convert 'r' to a 'Statement'and execute via current database connection.
execute :: forall m c r s dbv.
  ( HasOutputType s
  , MonadDb m Identity c dbv
  , MonadDbHasConn m c
  , ParseOutput dbv s
  , ToStatement r
  , s ~ S r
  )
  => r
  -> m (Either (ExecuteError (Error m Identity) dbv) (OutputT s))
execute =
  fmap extract . (askDbConn >>=)
  . flip (executeAndParse @_ @Identity) . pure . statement

-- | Like 'execute' but each 'Statement' is appended with a commit statement.
executeTx :: forall m c r s dbv.
  ( HasOutputType (Cons CommitTx s)
  , MonadDb m Identity c dbv
  , MonadDbWithConn m c
  , ParseOutput dbv (Cons CommitTx s)
  , ToStatement r
  , s ~ S r
  )
  => r
  -> m (Either (ExecuteError (Error m Identity) dbv) (OutputT (Cons CommitTx s)))
executeTx = fmap extract . withDbConn .
  flip (executeAndParse @_ @Identity) . pure . Statement.commitTx . statement

-- | Like 'executeTx' but shape of input and output is of type 't'.
executeTxs :: forall m t c r s dbv.
  ( HasOutputType (Cons CommitTx s)
  , MonadDb m t c dbv
  , MonadDbNewConn m c
  , ParseOutput dbv (Cons CommitTx s)
  , ToStatement r
  , s ~ S r
  )
  => t r
  -> m (t (Either (ExecuteError (Error m t) dbv) (OutputT (Cons CommitTx s))))
executeTxs =
  (newDbConn >>=) . flip executeAndParse . fmap (Statement.commitTx . statement)

-- | Run the provided actions, then run a commit statement.
tx :: forall m c a dbv.
  (MonadDb m Identity c dbv, MonadDbWithConn m c)
  => Tx m c (Either (ExecuteError (Error (Tx m c) Identity) dbv) a)
  -> m (Either (ExecuteError (Error (Tx m c) Identity) dbv) a)
tx m = withDbConn \c -> runTx c $ m >>= \case
  Left  e1 -> pure $ Left e1
  Right a  -> Database.Generic.Operations.execute Tx.CommitTx >>= \case
    Left  e2 -> pure $ Left e2
    Right () -> pure $ Right a

tx_ :: forall m c a dbv.
  (MonadDb m Identity c dbv, MonadDbWithConn m c, Show dbv, Typeable dbv)
  => Tx m c (Either (ExecuteError (Error (Tx m c) Identity) dbv) a)
  -> m a
tx_ = do
  let err e = error $
        "Error in Database.Generic.Operations.tx_: " <> displayException e
  fmap (fromEither err) . tx

-- * Internal.

-- | Slim wrapper over 'Db.execute' which also parses output.
executeAndParse :: forall m t c s dbv.
  (HasOutputType s, MonadDb m t c dbv, ParseOutput dbv s)
  => c
  -> t (Statement s)
  -> m (t (Either (ExecuteError (Error m t) dbv) (OutputT s)))
executeAndParse c ts = fmap f <$> Db.executeStatement' @_ @t c ts
 where
  f (Left  l) = Left l
  -- The 'mapLeft' lifts the 'OutputError' into 'ExecuteError'.
  f (Right o) = mapLeft from $ parse @dbv @s o
