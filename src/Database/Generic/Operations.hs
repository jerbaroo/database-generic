module Database.Generic.Operations where

import Database.Generic.Class (Error, MonadDb)
import Database.Generic.Class qualified as MonadDb
import Database.Generic.Entity (Entity)
import Database.Generic.Prelude

delete :: forall a f b m t.
  (Applicative t, Comonad t, Entity a f, Functor m, HasField f a b, MonadDb m t) =>
  b -> m (Either (Error m t) ())
delete = fmap (fmap extract) . deleteT @a @_ @_ @_ @t . pure

deleteT :: forall a f b m t.
  (Entity a f, HasField f a b, MonadDb m t) =>
  t b -> m (Either (Error m t) (t ()))
deleteT = MonadDb.delete @_ @_ @a

deleteI :: forall a f b m.
  (Entity a f, Functor m, HasField f a b, MonadDb m Identity) =>
  b -> m (Either (Error m Identity) ())
deleteI = delete @a @_ @_ @_ @Identity

select :: forall a f b m t.
  (Applicative t, Comonad t, Entity a f, Functor m, HasField f a b, MonadDb m t) =>
  b -> m (Either (Error m t) (Maybe a))
select = fmap (fmap $ fmap extract) . selectT @_ @_ @_ @_ @t . pure

selectT :: forall a f b m t.
  (Entity a f, HasField f a b, MonadDb m t) =>
  t b -> m (Either (Error m t) (Maybe (t a)))
selectT = MonadDb.select

selectI :: forall a f b m.
  (Entity a f, Functor m, HasField f a b, MonadDb m Identity) =>
  b -> m (Either (Error m Identity) (Maybe a))
selectI = select @a @_ @_ @_ @Identity

upsert :: forall a f m t.
  (Applicative t, Comonad t, Entity a f, Functor m, MonadDb m t) =>
  a -> m (Either (Error m t) ())
upsert = fmap (fmap extract) . upsertT @_ @_ @_ @t . pure

upsertT :: forall a f m t.
  (Entity a f, MonadDb m t) =>
  t a -> m (Either (Error m t) (t ()))
upsertT = MonadDb.upsert

upsertI :: forall a f m.
  (Entity a f, Functor m, MonadDb m Identity) =>
  a -> m (Either (Error m Identity) ())
upsertI = upsert @_ @_ @_ @Identity
