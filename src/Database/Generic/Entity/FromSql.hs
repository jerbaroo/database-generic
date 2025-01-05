{-# LANGUAGE UndecidableInstances #-}

module Database.Generic.Entity.FromSql where

import Data.ByteString.Char8 qualified as BS
import Database.Generic.Entity.SqlTypes (SqlValue(..))
import Database.Generic.Entity.ToSql (showType)
import Database.Generic.Prelude
import Generics.Eot qualified as G

data FromSqlError
  = ErrorConstructing()   ![SqlValue]
  | ErrorConstructingVoid ![SqlValue]
  | NoSqlValues           !String
  deriving Show

instance Exception FromSqlError

-- * SqlValue -> a

class FromSqlValue a where
  fromSqlValue :: SqlValue -> a

instance Convertible SqlValue a => FromSqlValue a where
  fromSqlValue = convert

-- * [SqlValue] -> a

class FromSqlValues a where
  fromSqlValues :: [SqlValue] -> a

instance FromSqlValues String where
  fromSqlValues [SqlString s] = s
  fromSqlValues [SqlByteString s] = BS.unpack s
  fromSqlValues xs = error $ "fromSqlValues @String got: " <> show xs

instance {-# OVERLAPPABLE #-} (G.HasEot a, GFromSqlValues (G.Eot a)) => FromSqlValues a where
  fromSqlValues = G.fromEot . gFromSqlValues

-- * Generic [SqlValue] -> a

class GFromSqlValues a where
  gFromSqlValues :: [SqlValue] -> a

instance (GFromSqlValues a, Typeable a, Typeable b) => GFromSqlValues (Either a b) where
  gFromSqlValues [] = throw $ NoSqlValues $ showType @(Either a b)
  gFromSqlValues xs = Left $ gFromSqlValues xs

instance (FromSqlValue a, GFromSqlValues as, Typeable a, Typeable as) => GFromSqlValues (a, as) where
  gFromSqlValues []     = throw $ NoSqlValues $ showType @(a, as)
  gFromSqlValues (x:xs) = (fromSqlValue x, gFromSqlValues xs)

-- | The end of the right-nested tuples.
instance GFromSqlValues () where
  gFromSqlValues [] = ()
  gFromSqlValues xs = throw $ ErrorConstructing() xs

-- | Necessary boilerplate.
instance GFromSqlValues G.Void where
  gFromSqlValues = throw . ErrorConstructingVoid
