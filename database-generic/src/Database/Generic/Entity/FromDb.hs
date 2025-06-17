{-# LANGUAGE UndecidableInstances #-}

module Database.Generic.Entity.FromDb where

import Database.Generic.Entity.DbColumns (HasDbColumns)
import Database.Generic.Entity.DbTypes (DbT(..), DbValue)
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

instance FromDbValues DbValue Bool where
  fromDbValues [DbBool b] = b
  fromDbValues x = error $ "Error constructing Bool from " <> show x

instance FromDbValues DbValue Int64 where
  fromDbValues [DbInt64   i] = i
  fromDbValues [DbInteger i] = unsafeFrom i
  fromDbValues x = error $ "Error constructing Int64 from " <> show x

instance FromDbValues DbValue String where
  fromDbValues [DbBytes  b] = from b
  fromDbValues [DbString s] = s
  fromDbValues x = error $ "Error constructing Int64 from " <> show x

instance {-# OVERLAPPABLE #-}
  ( G.HasEot a
  , GFromDbValues dbv (G.Eot a)
  , HasDbColumns a -- Only included to ensure that 'FromDbValues' instances aren't
                   -- derived for simple datatypes such as 'Bool'.
  ) => FromDbValues dbv a where
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
