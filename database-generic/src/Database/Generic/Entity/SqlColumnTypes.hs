{-# LANGUAGE UndecidableInstances #-}

module Database.Generic.Entity.SqlColumnTypes where

import Database.Generic.Entity.FieldName (FieldName, HasFieldNames(..))
import Database.Generic.Entity.SqlTypes (HasSqlType(sqlType), SqlType)
import Database.Generic.Prelude
import Generics.Eot qualified as G

-- | SQL type for each field of 'a'.
class HasSqlColumnTypes a where
  sqlColumnTypes :: [SqlType]

instance (G.HasEot a, GHasSqlColumnTypes G.Datatype (G.Eot a))
  => HasSqlColumnTypes a where
  sqlColumnTypes = gSqlColumnTypes @_ @(G.Eot a) $ G.datatype $ Proxy @a

class GHasSqlColumnTypes meta a where
  gSqlColumnTypes :: meta -> [SqlType]

instance GHasSqlColumnTypes [String] fields
  => GHasSqlColumnTypes G.Datatype (Either fields G.Void) where
  gSqlColumnTypes datatype =
    case datatype of
      G.Datatype _ [G.Constructor _ (G.Selectors fields)] ->
        gSqlColumnTypes @_ @fields fields
      G.Datatype name [G.Constructor cName (G.NoSelectors _)] ->
        error $ name <> " constructor " <> cName <> " has no selectors"
      G.Datatype name _ -> error $ name <> " must have exactly one constructor"

instance (HasSqlType a, GHasSqlColumnTypes [String] as)
  => GHasSqlColumnTypes [String] (a, as) where
  gSqlColumnTypes (_:as) = sqlType @a : gSqlColumnTypes @_ @as as
  gSqlColumnTypes []     = error "impossible"

instance GHasSqlColumnTypes [String] () where
  gSqlColumnTypes [] = []
  gSqlColumnTypes _  = error "impossible"

-- | Values that have both named a SQL field and SQL type for each field.
class (HasFieldNames a, HasSqlColumnTypes a) => HasSqlFields a where
  sqlFields :: [(FieldName, SqlType)]

instance (HasFieldNames a, HasSqlColumnTypes a) => HasSqlFields a where
  sqlFields = zip (fieldNames @a) (sqlColumnTypes @a)
