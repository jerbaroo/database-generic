module Database.Generic.Operations where

import Database.Generic.Class (Error, MonadDb)
import Database.Generic.Class qualified as MonadDb
import Database.Generic.Entity (Entity)
import Database.Generic.Prelude
import Database.Generic.Statement qualified as Statement

-- * Create table.

createTable :: forall a f m.
  (Entity f a, Functor m, MonadDb m Identity) =>
  Bool -> m (Either (Error m Identity) ())
createTable = fmap (fmap extract) . createTableT @a @_ @_ @Identity . pure

createTableT :: forall a f m t.
  (Entity f a, Functor t, MonadDb m t) =>
  t Bool -> m (Either (Error m t) (t ()))
createTableT = MonadDb.createTable . fmap (Statement.createTable @a)

-- * Delete.

delete :: forall a f b m.
  (Entity f a, Functor m, HasField f a b, MonadDb m Identity) =>
  b -> m (Either (Error m Identity) ())
delete = fmap (fmap extract) . deleteT @a @_ @_ @_ @Identity . pure

deleteT :: forall a f b m t.
  (Entity f a, HasField f a b, MonadDb m t) =>
  t b -> m (Either (Error m t) (t ()))
deleteT = MonadDb.delete @_ @_ @a

-- * Select.

select :: forall a f b m.
  (Entity f a, Functor m, HasField f a b, MonadDb m Identity) =>
  b -> m (Either (Error m Identity) (Maybe a))
select = fmap (fmap $ fmap extract) . selectT @_ @_ @_ @_ @Identity . pure

selectT :: forall a f b m t.
  (Entity f a, HasField f a b, MonadDb m t) =>
  t b -> m (Either (Error m t) (Maybe (t a)))
selectT = MonadDb.select

-- * Upsert.

upsert :: forall a f m.
  (Entity f a, Functor m, MonadDb m Identity) =>
  a -> m (Either (Error m Identity) ())
upsert = fmap (fmap extract) . upsertT @_ @_ @_ @Identity . pure

upsertT :: forall a f m t.
  (Entity f a, MonadDb m t) =>
  t a -> m (Either (Error m t) (t ()))
upsertT = MonadDb.upsert
