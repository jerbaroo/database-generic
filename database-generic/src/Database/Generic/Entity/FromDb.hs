{-# LANGUAGE UndecidableInstances #-}

module Database.Generic.Entity.FromDb where

import Data.ByteString.Char8 qualified as BS
import Database.Generic.Entity.DbColumns (HasDbColumns)
import Database.Generic.Entity.DbTypes (Bytes(..), DbT(..), DbValueN)
import Database.Generic.Prelude
import Generics.Eot qualified as G
import GHC.Num (Num(fromInteger))

data FromDbError dbv
  = ErrorConstructing()   ![dbv]
  | ErrorConstructingVoid ![dbv]
  | NoDbValues            !String
  deriving Show

instance (Show dbv, Typeable dbv) => Exception (FromDbError dbv)

-- | Values that can be parsed from a list of 'dbv'.
class FromDbValues dbv a where
  fromDbValues :: [dbv] -> a

instance FromDbValues DbValueN Bool where
  fromDbValues [Just (DbBool b)] = b
  fromDbValues x = error $ "Error constructing Bool from " <> show x

instance FromDbValues DbValueN Int64 where
  fromDbValues [Just (DbInt64   i)] = i
  fromDbValues [Just (DbInteger i)] = fromInteger i
  fromDbValues x = error $ "Error constructing Int64 from " <> show x

instance FromDbValues DbValueN String where
  fromDbValues [Just (DbBytes (Bytes b))] = BS.unpack b
  fromDbValues [Just (DbString s)] = s
  fromDbValues x = error $ "Error constructing Int64 from " <> show x

instance (FromDbValues DbValueN a, Typeable a) => FromDbValues DbValueN (Maybe a) where
  fromDbValues [Nothing ] = Nothing
  fromDbValues [Just dbv] = Just $ fromDbValues @DbValueN @a [Just dbv]
  fromDbValues x = error $ "Error constructing Maybe " <> showType @a <> " from " <> show x

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
