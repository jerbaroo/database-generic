{-# LANGUAGE UndecidableInstances #-}

module Database.Generic.Entity.ToSql where

import Database.Generic.Entity.SqlTypes (SqlValue)
import Database.Generic.Prelude
import Generics.Eot qualified as G

newtype ToSqlValuesError = MoreThanOneConstructor String deriving Show

instance Exception ToSqlValuesError

-- | Values that can be converted into a single 'SqlValue'.
class ToSqlValue a where
  toSqlValue :: a -> SqlValue

instance {-# OVERLAPPABLE #-} From a SqlValue => ToSqlValue a where
  toSqlValue = from

-- | Values that can be converted into a list of 'SqlValue'.
class ToSqlValues a where
  toSqlValues :: a -> [SqlValue]

instance {-# OVERLAPPABLE #-} (G.HasEot a, GToSqlValues (G.Eot a)) => ToSqlValues a where
  toSqlValues = gToSqlValues . G.toEot

-- | Typeclass for generic implementation of 'ToSqlValues'.
class GToSqlValues a where
  gToSqlValues :: a -> [SqlValue]

-- | Convert the first data constructor's fields to '[SqlValue]'.
--
-- Only operate on the first constructor. Error in case of a 'Right', which
-- means that a data type has more than one constructor.
instance (GToSqlValues a, Typeable b) => GToSqlValues (Either a b) where
  gToSqlValues (Left  fields) = gToSqlValues fields
  gToSqlValues (Right _     ) = throw $ MoreThanOneConstructor $ showType @b

-- | Convert a data type's fields to `[SqlValue]`.
--
-- Each left value of the 2-tuple represents one field of a data type. The
-- 2-tuples are right-nested, so the remaining fields are nested in 'as'.
instance (ToSqlValue a, GToSqlValues as) => GToSqlValues (a, as) where
  gToSqlValues (a, as) = toSqlValue a : gToSqlValues as

instance GToSqlValues () where
  gToSqlValues _ = []

instance GToSqlValues G.Void where
  gToSqlValues = G.absurd
