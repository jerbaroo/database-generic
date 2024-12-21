{-# LANGUAGE UndecidableInstances #-}

module Database.Generic.Entity.ToSql where

import Database.Generic.Prelude
import Database.Generic.Entity.SqlValue (SqlValue)
import Generics.Eot qualified as G

-- | Type of 'a' as a 'String'.
showType :: forall a. Typeable a => String
showType = show $ typeRep $ Proxy @a

-- | Like 'showType' but with quotes removed.
showType' :: forall a. Typeable a => String
showType' = replace "\"" "" $ showType @a

data ToSqlError
  = MoreThanOneConstructor String
  | NoConstructors         String
  | NoFields               String
  | NoSelectors            String
  deriving Show

instance Exception ToSqlError

-- * a -> SqlValue

class ToSqlValue a where
  toSqlValue :: a -> SqlValue

instance Convertible a SqlValue => ToSqlValue a where
  toSqlValue = convert

-- * a -> [SqlValue]

class ToSqlValues a where
  toSqlValues :: a -> [SqlValue]

-- | Generic instance for 'ToSqlValues'.
instance (G.HasEot a, GToSqlValues (G.Eot a)) => ToSqlValues a where
  toSqlValues = gToSqlValues . G.toEot

-- * Generic a -> [SqlValue]

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
  gToSqlValues () = []

instance GToSqlValues G.Void where
  gToSqlValues = G.absurd

-- * a -> [(String, SqlValue)]

class ToSqlFields a where
  toSqlFields :: a -> [(String, SqlValue)]

instance (G.HasEot a, GToSqlValues (G.Eot a), Typeable a) => ToSqlFields a where
  toSqlFields a = zip (fieldNames a) (toSqlValues a)

-- | The names of a data type's fields.
fieldNames :: forall a. (G.HasEot a, Typeable a) => a -> [String]
fieldNames _ =
  case G.constructors $ G.datatype $ Proxy @a of
    []  -> throw $ NoConstructors $ showType @a
    [c] -> case G.fields c of
      G.Selectors   fields -> fields
      G.NoSelectors _      -> throw $ NoSelectors $ showType @a
      G.NoFields           -> throw $ NoFields $ showType @a
    _ -> throw $ MoreThanOneConstructor $ showType @a
