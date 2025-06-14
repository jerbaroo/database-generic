{-# LANGUAGE UndecidableInstances #-}

module Database.Generic.Entity.ToSql where

import Database.Generic.Entity.SqlTypes (DbValue)
import Database.Generic.Prelude
import Generics.Eot qualified as G

newtype ToSqlValuesError = MoreThanOneConstructor String deriving Show

instance Exception ToSqlValuesError

-- | Values that can be converted into a single 'SqlValue'.
class ToDbValue a where
  toDbValue :: a -> DbValue

instance {-# OVERLAPPABLE #-} From a DbValue => ToDbValue a where
  toDbValue = from

-- | Values that can be converted into a list of 'DbValue'.
class ToDbValues a where
  toDbValues :: a -> [DbValue]

instance {-# OVERLAPPABLE #-} (G.HasEot a, GToDbValues (G.Eot a)) => ToDbValues a where
  toDbValues = gToDbValues . G.toEot

-- | Typeclass for generic implementation of 'ToDbValues'.
class GToDbValues a where
  gToDbValues :: a -> [DbValue]

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
