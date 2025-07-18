{-# LANGUAGE UndecidableInstances #-}

module Database.Generic.Entity.ToDb where

import Database.Generic.Entity.DbColumns (HasDbColumns)
import Database.Generic.Entity.DbTypes (DbT(..), DbValueN)
import Database.Generic.Prelude
import Generics.Eot qualified as G

newtype ToDbValuesError = MoreThanOneConstructor String deriving Show

instance Exception ToDbValuesError

-- | Values that can be converted into a single 'DbValueN'.
class ToDbValue a where
  toDbValue :: a -> DbValueN

instance ToDbValue Bool   where toDbValue = Just . DbBool
instance ToDbValue Int64  where toDbValue = Just . DbInt64
instance ToDbValue String where toDbValue = Just . DbString

instance ToDbValue a => ToDbValue (Maybe a) where
  toDbValue Nothing  = Nothing
  toDbValue (Just a) = toDbValue a

-- | Values that can be converted into a list of 'DbValueN'.
class ToDbValues a where
  toDbValues :: a -> [DbValueN]

instance {-# OVERLAPPABLE #-} ToDbValue (Maybe a) => ToDbValues (Maybe a) where
  toDbValues = (:[]) . toDbValue

instance {-# OVERLAPPABLE #-}
  ( G.HasEot a
  , GToDbValues (G.Eot a)
  , HasDbColumns a -- Only included to ensure that 'ToDbValues' instances aren't
                   -- derived for simple datatypes such as 'Bool'.
  ) => ToDbValues a where
  toDbValues = gToDbValues . G.toEot

-- | Typeclass for generic implementation of 'ToDbValues'.
class GToDbValues a where
  gToDbValues :: a -> [DbValueN]

-- | Convert the first data constructor's fields to '[DbValue]'.
--
-- Only operate on the first constructor. Error in case of a 'Right', which
-- means that a data type has more than one constructor.
instance (GToDbValues a, Typeable b) => GToDbValues (Either a b) where
  gToDbValues (Left  fields) = gToDbValues fields
  gToDbValues (Right _     ) = throw $ MoreThanOneConstructor $ showType @b

-- | Convert a data type's fields to `[DbValue]`.
--
-- Each left value of the 2-tuple represents one field of a data type. The
-- 2-tuples are right-nested, so the remaining fields are nested in 'as'.
instance (ToDbValue a, GToDbValues as) => GToDbValues (a, as) where
  gToDbValues (a, as) = toDbValue a : gToDbValues as

instance GToDbValues () where
  gToDbValues _ = []

instance GToDbValues G.Void where
  gToDbValues = G.absurd
