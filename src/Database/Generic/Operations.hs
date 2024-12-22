module Database.Generic.Operations where

import Database.Generic.Class (Error, MonadDb, MonadDbActiveConn(..))
import Database.Generic.Class qualified as MonadDb
import Database.Generic.Entity (Entity)
import Database.Generic.Prelude
import Database.Generic.Statement qualified as Statement

-- * Create table

createTable :: forall a f m c.
  (Entity f a, Functor m, Monad m, MonadDb m Identity c, MonadDbActiveConn m c) =>
  Bool -> m (Either (Error m Identity c) ())
createTable = fmap (fmap extract) . createTableT @a @_ @_ @Identity @c . pure

createTableT :: forall a f m t c.
  (Entity f a, Functor t, Monad m, MonadDb m t c, MonadDbActiveConn m c) =>
  t Bool -> m (Either (Error m t c) (t ()))
createTableT ts = activeConn @_ @c >>=
  (flip (MonadDb.createTable @_ @_ @c) $ fmap (Statement.createTable @a) ts)

-- -- * Delete

-- delete :: forall a f b m c.
--   (Entity f a, Functor m, HasField f a b, MonadDb m Identity c) =>
--   b -> m (Either (Error m Identity c) ())
-- delete = fmap (fmap extract) . deleteT @a @_ @_ @_ @Identity . pure

-- deleteT :: forall a f b m t c.
--   (Entity f a, HasField f a b, MonadDb m t c) =>
--   t b -> m (Either (Error m t c) (t ()))
-- deleteT = MonadDb.delete @_ @_ @a

-- -- * Select

-- select :: forall a f b m c.
--   (Entity f a, Functor m, HasField f a b, MonadDb m Identity c) =>
--   b -> m (Either (Error m Identity c) (Maybe a))
-- select = fmap (fmap $ fmap extract) . selectT @_ @_ @_ @_ @Identity . pure

-- selectT :: forall a f b m t c.
--   (Entity f a, HasField f a b, MonadDb m t c) =>
--   t b -> m (Either (Error m t c) (Maybe (t a)))
-- selectT = MonadDb.select

-- -- * Upsert

-- upsert :: forall a f m c.
--   (Entity f a, Functor m, MonadDb m Identity c) =>
--   a -> m (Either (Error m Identity c) ())
-- upsert = fmap (fmap extract) . upsertT @_ @_ @_ @Identity . pure

-- upsertT :: forall a f m t c.
--   (Entity f a, MonadDb m t c) =>
--   t a -> m (Either (Error m t c) (t ()))
-- upsertT = MonadDb.upsert
