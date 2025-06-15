{-# LANGUAGE UndecidableInstances #-}

module Database.Generic.Entity.FromDb where

import Database.Generic.Prelude
import Generics.Eot qualified as G

data FromDbError dbv
  = ErrorConstructing()   ![dbv]
  | ErrorConstructingVoid ![dbv]
  | NoDbValues            !String
  deriving Show

instance (Show dbv, Typeable dbv) => Exception (FromDbError dbv)

-- | Values that can be parsed from a list of 'dbv'.
class FromDbValues dbv a where
  fromDbValues :: [dbv] -> a

instance {-# OVERLAPPABLE #-} (G.HasEot a, GFromDbValues dbv (G.Eot a)) => FromDbValues dbv a where
  fromDbValues = G.fromEot . gFromDbValues

-- | Typeclass for generic implementation of 'FromDbValues'.
class GFromDbValues dbv a where
  gFromDbValues :: [dbv] -> a

instance (GFromDbValues dbv a, Show dbv, Typeable a, Typeable b, Typeable dbv) => GFromDbValues dbv (Either a b) where
  gFromDbValues [] = throw $ NoDbValues @dbv $ showType @(Either a b)
  gFromDbValues xs = Left $ gFromDbValues xs

instance (FromDbValues dbv a, GFromDbValues dbv as, Show dbv, Typeable a, Typeable as, Typeable dbv) => GFromDbValues dbv (a, as) where
  gFromDbValues []     = throw $ NoDbValues @dbv $ showType @(a, as)
  gFromDbValues (x:xs) = (fromDbValues [x], gFromDbValues xs)

-- | The end of the right-nested tuples.
instance (Show dbv, Typeable dbv) => GFromDbValues dbv () where
  gFromDbValues [] = ()
  gFromDbValues xs = throw $ ErrorConstructing() xs

-- | Necessary boilerplate.
instance (Show dbv, Typeable dbv) => GFromDbValues dbv G.Void where
  gFromDbValues = throw . ErrorConstructingVoid
