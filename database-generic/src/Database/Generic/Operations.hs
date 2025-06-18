-- TODO docs
module Database.Generic.Operations where

import Database.Generic.Class (ExecuteReturns, MonadDb(..), MonadDbHasConn(..), MonadDbNewConn(..), MonadDbWithConn(..))
import Database.Generic.Class qualified as Db
import Database.Generic.Database (Database, DbV)
import Database.Generic.Prelude
import Database.Generic.Statement (Statement(..), ToStatement(..))
import Database.Generic.Statement qualified as Statement
import Database.Generic.Statement.Output (HasOutputType, OutputT, ParseOutput(..))
import Database.Generic.Statement.Type (List(..), StatementType(..))
import Database.Generic.Statement.Tx qualified as Tx
import Database.Generic.Transaction (Tx, runTx)
import Witch (From(from))

-- | Convert 'r' to a 'Statement'and execute via current database connection.
execute :: forall m r s db.
  ( Database db
  , HasOutputType s
  , MonadDb m db
  , MonadDbHasConn m (C m db)
  , ParseOutput (DbV db) s
  , ToStatement r
  , s ~ S r
  )
  => r
  -> m (ExecuteReturns m db (OutputT s))
execute =
  fmap extract . (askDbConn >>=) . flip executeAndParse . pure . statement

-- | Like 'execute' but each 'Statement' is appended with a commit statement.
executeTx :: forall m r s db.
  ( Database db
  , HasOutputType (L CommitTx s)
  , MonadDb m db
  , MonadDbWithConn m (C m db)
  , ParseOutput (DbV db) (L CommitTx s)
  , ToStatement r
  , s ~ S r
  )
  => r
  -> m (ExecuteReturns m db (OutputT (L CommitTx s)))
executeTx =
  fmap extract . withDbConn . flip executeAndParse . pure . Statement.commitTx . statement

-- | Like 'executeTx' but shape of input and output is of type 't'.
executeTxs :: forall m r s db.
  ( Database db
  , HasOutputType (L CommitTx s)
  , MonadDb m db
  , MonadDbNewConn m (C m db)
  , ParseOutput (DbV db) (L CommitTx s)
  , ToStatement r
  , s ~ S r
  )
  => (T m db) r
  -> m ((T m db) (ExecuteReturns m db (OutputT (L CommitTx s))))
executeTxs =
  (newDbConn >>=) . flip executeAndParse . fmap (Statement.commitTx . statement)

-- | Run the provided actions, then run a commit statement.
tx :: forall m a db
  . ( Database db
    , MonadDb m db
    , MonadDbWithConn m (C m db)
    )
  => Tx m (C m db) (ExecuteReturns (Tx m (C m db)) db a)
  -> m (ExecuteReturns (Tx m (C m db)) db a)
tx m = withDbConn \c -> runTx c $ m >>= \case
  Left  e1 -> pure $ Left e1
  Right a  -> Database.Generic.Operations.execute Tx.CommitTx >>= \case
    Left  e2 -> pure $ Left e2
    Right () -> pure $ Right a

tx_ :: forall m a db
  . ( Database db
    , MonadDb m db
    , MonadDbWithConn m (C m db)
    , Show (DbV db)
    , Typeable (DbV db)
    )
  => Tx m (C m db) (ExecuteReturns (Tx m (C m db)) db a)
  -> m a
tx_ = do
  let err e = error $
        "Error in Database.Generic.Operations.tx_: " <> displayException e
  fmap (fromEither err) . tx

-- * Internal.

-- | Slim wrapper over 'Db.execute' which also parses output.
executeAndParse :: forall m s db.
  (Database db, HasOutputType s, MonadDb m db, ParseOutput (DbV db) s)
  => C m db
  -> (T m db) (Statement s)
  -> m ((T m db) (ExecuteReturns m db (OutputT s)))
executeAndParse c ts = fmap f <$> Db.executeStatement' c ts
 where
  f (Left  l) = Left l
  -- The 'mapLeft' lifts the 'OutputError' into 'ExecuteError'.
  f (Right o) = mapLeft from $ parse @_ @s o
