module Database.Generic.Operations where

import Database.Generic.Entity (Entity)
import Database.Generic.Class (MonadDB)

select :: forall f m a b.
  (Applicative f, Comonad f, Entity a b, MonadDB m f) =>
  b -> m (Maybe e)
select = fmap (fmap extract) . selectByIdF @_ @c @f . pure

-- selectByIdF :: forall m c f e i.
--   (Entity e, EntityId e i, MonadGP m c f) => f i -> m (Maybe (f e))
-- selectByIdF i = GP.askConn @_ @c >>= flip GP.selectById i
