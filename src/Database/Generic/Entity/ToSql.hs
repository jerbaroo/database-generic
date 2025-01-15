{-# LANGUAGE UndecidableInstances #-}

module Database.Generic.Entity.ToSql where

import Database.Generic.Entity.FieldName (FieldName(..), HasFieldNames(..))
import Database.Generic.Entity.SqlTypes (SqlTypeId(..), SqlValue)
import Database.Generic.Prelude
import Generics.Eot qualified as G

data ToSqlError
  = MoreThanOneConstructor !String
  | NoConstructors         !String
  | NoFields               !String
  | NoSelectors            !String
  deriving Show

instance Exception ToSqlError

-- | Values that can be converted into a single 'SqlValue'.
class ToSqlValue a where
  toSqlValue :: a -> SqlValue

instance {-# OVERLAPPABLE #-} Convertible a SqlValue => ToSqlValue a where
  toSqlValue = convert

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

-- | Types that have a corresponding SQL type.
class HasSqlType a where
  sqlType :: SqlTypeId

instance HasSqlType Int64 where
  sqlType = SqlBigIntT

instance HasSqlType String where
  sqlType = SqlLongVarCharT

-- | Types that have an SQL type for each field.
class HasSqlColumnTypes a where
  sqlColumnTypes :: [SqlTypeId]

instance (G.HasEot a, GHasSqlColumnTypes G.Datatype (G.Eot a)) => HasSqlColumnTypes a where
  sqlColumnTypes = gSqlColumnTypes @_ @(G.Eot a) $ G.datatype $ Proxy @a

class GHasSqlColumnTypes meta a where
  gSqlColumnTypes :: meta -> [SqlTypeId]

instance GHasSqlColumnTypes [String] fields => GHasSqlColumnTypes G.Datatype (Either fields G.Void) where
  gSqlColumnTypes datatype = case datatype of
    G.Datatype _ [G.Constructor _ (G.Selectors fields)] ->
      gSqlColumnTypes @_ @fields fields
    G.Datatype name [G.Constructor cName (G.NoSelectors _)] ->
      error $ name <> " constructor " <> cName <> " has no selectors"
    G.Datatype name _ -> error $ name <> " must have exactly one constructor"

instance (HasSqlType a, GHasSqlColumnTypes [String] as) => GHasSqlColumnTypes [String] (a, as) where
  gSqlColumnTypes (_:as) = sqlType @a : gSqlColumnTypes @_ @as as
  gSqlColumnTypes []     = error "impossible"

instance GHasSqlColumnTypes [String] () where
  gSqlColumnTypes [] = []
  gSqlColumnTypes _  = error "impossible"

-- | Values that have both named a SQL column and SQL type for each field.
class (HasFieldNames a, HasSqlColumnTypes a) => HasSqlColumns a where
  sqlColumns :: [(FieldName, SqlTypeId)]

instance (HasFieldNames a, HasSqlColumnTypes a) => HasSqlColumns a where
  sqlColumns = zip (fieldNames @a) (sqlColumnTypes @a)
