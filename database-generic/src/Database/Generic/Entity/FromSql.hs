{-# LANGUAGE UndecidableInstances #-}

module Database.Generic.Entity.FromSql where

import Database.Generic.Entity.SqlTypes (SqlBS(..), SqlValue(..))
import Database.Generic.Prelude
import Generics.Eot qualified as G

data FromSqlError
  = ErrorConstructing()   ![SqlValue]
  | ErrorConstructingVoid ![SqlValue]
  | NoSqlValues           !String
  deriving Show

instance Exception FromSqlError

-- | Values that can be parsed from 'SqlValue's.
class FromSqlValues a where
  fromSqlValues :: [SqlValue] -> a

instance FromSqlValues String where
  fromSqlValues [SqlString s] = s
  fromSqlValues [SqlByteString (SqlBS b)] = unsafeFrom $ into @Utf8S b -- TODO why?
  fromSqlValues xs = error $ "fromSqlValues @String got: " <> show xs

instance FromSqlValues Int64 where
  fromSqlValues [SqlInteger i] = unsafeFrom i
  fromSqlValues [SqlInt64 i] = i
  fromSqlValues xs = error $ "fromSqlValues @Int64 got: " <> show xs

instance {-# OVERLAPPABLE #-} (G.HasEot a, GFromSqlValues (G.Eot a)) => FromSqlValues a where
  fromSqlValues = G.fromEot . gFromSqlValues

-- | Typeclass for generic implementation of 'FromSqlValues'.
class GFromSqlValues a where
  gFromSqlValues :: [SqlValue] -> a

instance (GFromSqlValues a, Typeable a, Typeable b) => GFromSqlValues (Either a b) where
  gFromSqlValues [] = throw $ NoSqlValues $ showType @(Either a b)
  gFromSqlValues xs = Left $ gFromSqlValues xs

instance (FromSqlValues a, GFromSqlValues as, Typeable a, Typeable as) => GFromSqlValues (a, as) where
  gFromSqlValues []     = throw $ NoSqlValues $ showType @(a, as)
  gFromSqlValues (x:xs) = (fromSqlValues [x], gFromSqlValues xs)

-- | The end of the right-nested tuples.
instance GFromSqlValues () where
  gFromSqlValues [] = ()
  gFromSqlValues xs = throw $ ErrorConstructing() xs

-- | Necessary boilerplate.
instance GFromSqlValues G.Void where
  gFromSqlValues = throw . ErrorConstructingVoid
