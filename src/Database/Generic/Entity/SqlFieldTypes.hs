{-# LANGUAGE UndecidableInstances #-}

module Database.Generic.Entity.SqlFieldTypes where

import Database.Generic.Entity.FieldName (FieldName, HasFieldNames(..))
import Database.Generic.Entity.SqlTypes (SqlType(sqlType), SqlTypeId)
import Database.Generic.Prelude
import Generics.Eot qualified as G

-- | SQL type for each field of 'a'.
class HasSqlFieldTypes a where
  sqlFieldTypes :: [SqlTypeId]

instance (G.HasEot a, GHasSqlFieldTypes G.Datatype (G.Eot a))
  => HasSqlFieldTypes a where
  sqlFieldTypes = gSqlFieldTypes @_ @(G.Eot a) $ G.datatype $ Proxy @a

class GHasSqlFieldTypes meta a where
  gSqlFieldTypes :: meta -> [SqlTypeId]

instance GHasSqlFieldTypes [String] fields
  => GHasSqlFieldTypes G.Datatype (Either fields G.Void) where
  gSqlFieldTypes datatype =
    case datatype of
      G.Datatype _ [G.Constructor _ (G.Selectors fields)] ->
        gSqlFieldTypes @_ @fields fields
      G.Datatype name [G.Constructor cName (G.NoSelectors _)] ->
        error $ name <> " constructor " <> cName <> " has no selectors"
      G.Datatype name _ -> error $ name <> " must have exactly one constructor"

instance (SqlType a, GHasSqlFieldTypes [String] as)
  => GHasSqlFieldTypes [String] (a, as) where
  gSqlFieldTypes (_:as) = sqlType @a : gSqlFieldTypes @_ @as as
  gSqlFieldTypes []     = error "impossible"

instance GHasSqlFieldTypes [String] () where
  gSqlFieldTypes [] = []
  gSqlFieldTypes _  = error "impossible"

-- | Values that have both named a SQL field and SQL type for each field.
class (HasFieldNames a, HasSqlFieldTypes a) => HasSqlFields a where
  sqlFields :: [(FieldName, SqlTypeId)]

instance (HasFieldNames a, HasSqlFieldTypes a) => HasSqlFields a where
  sqlFields = zip (fieldNames @a) (sqlFieldTypes @a)
