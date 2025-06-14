{-# LANGUAGE UndecidableInstances #-}

module Database.Generic.Entity.FromSql where

import Database.Generic.Entity.SqlTypes (DbT(..), DbValue)
import Database.Generic.Prelude
import Generics.Eot qualified as G

data FromSqlError
  = ErrorConstructing()   ![DbValue]
  | ErrorConstructingVoid ![DbValue]
  | NoDbValues           !String
  deriving Show

instance Exception FromSqlError

-- | Values that can be parsed from 'DbValue's.
class FromDbValues a where
  fromDbValues :: [DbValue] -> a

instance FromDbValues String where
  fromDbValues [DbString s] = s
  -- fromDbValues [DbByteString (SqlBS b)] = unsafeFrom $ into @Utf8S b -- TODO why?
  fromDbValues xs = error $ "fromDbValues @String got: " <> show xs

instance FromDbValues Int64 where
  -- fromDbValues [DbInteger i] = unsafeFrom i
  fromDbValues [DbInt64 i] = i
  fromDbValues xs = error $ "fromDbValues @Int64 got: " <> show xs

instance {-# OVERLAPPABLE #-} (G.HasEot a, GFromDbValues (G.Eot a)) => FromDbValues a where
  fromDbValues = G.fromEot . gFromDbValues

-- | Typeclass for generic implementation of 'FromDbValues'.
class GFromDbValues a where
  gFromDbValues :: [DbValue] -> a

instance (GFromDbValues a, Typeable a, Typeable b) => GFromDbValues (Either a b) where
  gFromDbValues [] = throw $ NoDbValues $ showType @(Either a b)
  gFromDbValues xs = Left $ gFromDbValues xs

instance (FromDbValues a, GFromDbValues as, Typeable a, Typeable as) => GFromDbValues (a, as) where
  gFromDbValues []     = throw $ NoDbValues $ showType @(a, as)
  gFromDbValues (x:xs) = (fromDbValues [x], gFromDbValues xs)

-- | The end of the right-nested tuples.
instance GFromDbValues () where
  gFromDbValues [] = ()
  gFromDbValues xs = throw $ ErrorConstructing() xs

-- | Necessary boilerplate.
instance GFromDbValues G.Void where
  gFromDbValues = throw . ErrorConstructingVoid
