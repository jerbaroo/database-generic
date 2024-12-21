{-# LANGUAGE UndecidableInstances #-}

module Database.Generic.Entity.ToSql where

import Database.Generic.Prelude
import Database.Generic.Entity.SqlTypes (SqlType(..), SqlValue)
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
  gToSqlValues _ = []

instance GToSqlValues G.Void where
  gToSqlValues = G.absurd

-- * @a [String]

class HasSqlFieldNames a where
  sqlFieldNames :: [String]

instance (G.HasEot a, Typeable a) => HasSqlFieldNames a where
  sqlFieldNames =
    case G.constructors $ G.datatype $ Proxy @a of
      []  -> throw $ NoConstructors $ showType @a
      [c] -> case G.fields c of
        G.Selectors   fields -> fields
        G.NoSelectors _      -> throw $ NoSelectors $ showType @a
        G.NoFields           -> throw $ NoFields $ showType @a
      _ -> throw $ MoreThanOneConstructor $ showType @a

-- * @a SqlType

class HasSqlType a where
  sqlType :: SqlType

instance HasSqlType Int where
  sqlType = SqlInt64

instance HasSqlType String where
  sqlType = SqlString

-- * @a [SqlType]

class HasSqlFieldTypes a where
  sqlFieldTypes :: [SqlType]

instance (G.HasEot a, GHasSqlFieldTypes G.Datatype (G.Eot a)) => HasSqlFieldTypes a where
  sqlFieldTypes = gSqlFieldTypes @_ @(G.Eot a) $ G.datatype $ Proxy @a

-- * Generic Proxy a -> [SqlType]

class GHasSqlFieldTypes meta a where
  gSqlFieldTypes :: meta -> [SqlType]

instance GHasSqlFieldTypes [String] fields => GHasSqlFieldTypes G.Datatype (Either fields G.Void) where
  gSqlFieldTypes datatype = case datatype of
    G.Datatype _ [G.Constructor _ (G.Selectors fields)] ->
      gSqlFieldTypes @_ @fields fields
    G.Datatype name [G.Constructor cName (G.NoSelectors _)] ->
      error $ name <> " constructor " <> cName <> " has no selectors"
    G.Datatype name _ -> error $ name <> " must have exactly one constructor"

instance (HasSqlType a, GHasSqlFieldTypes [String] as) => GHasSqlFieldTypes [String] (a, as) where
  gSqlFieldTypes (_:as) = sqlType @a : gSqlFieldTypes @_ @as as
  gSqlFieldTypes []     = error "impossible"

instance GHasSqlFieldTypes [String] () where
  gSqlFieldTypes [] = []
  gSqlFieldTypes _  = error "impossible"
