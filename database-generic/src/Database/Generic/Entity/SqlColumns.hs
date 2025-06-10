{-# LANGUAGE UndecidableInstances #-}

module Database.Generic.Entity.SqlColumns where

import Database.Generic.Entity.FieldName (FieldName(..), FieldNameTransformation(..))
import Database.Generic.Entity.SqlTypes (HasSqlType(sqlType), SqlType)
import Database.Generic.Prelude
import Generics.Eot qualified as G

-- | Name of column and database column type for each field of 'a'.
class HasSqlColumns a where
  sqlColumns :: [(FieldName, SqlType)]

instance (G.HasEot a, GHasSqlColumns a G.Datatype (G.Eot a))
  => HasSqlColumns a where
  sqlColumns = gSqlColumns @a @_ @(G.Eot a) $ G.datatype $ Proxy @a

class GHasSqlColumns a meta ga where
  gSqlColumns :: meta -> [(FieldName, SqlType)]

instance GHasSqlColumns a [String] fields
  => GHasSqlColumns a G.Datatype (Either fields G.Void) where
  gSqlColumns datatype =
    case datatype of
      G.Datatype _ [G.Constructor _ (G.Selectors fields)] ->
        gSqlColumns @a @_ @fields fields
      G.Datatype name [G.Constructor cName (G.NoSelectors _)] ->
        error $ name <> " constructor " <> cName <> " has no selectors"
      G.Datatype name _ -> error $ name <> " must have exactly one constructor"

instance (HasSqlType f, GHasSqlColumns a [String] fs)
  => GHasSqlColumns a [String] (f, fs) where
  gSqlColumns []     = error "impossible"
  gSqlColumns (f:fs) =
    (fieldNameT $ FieldName f, sqlType @f) : gSqlColumns @a @_ @fs fs

instance GHasSqlColumns a [String] () where
  gSqlColumns [] = []
  gSqlColumns _  = error "impossible"

fieldNames :: forall a. HasSqlColumns a => [FieldName]
fieldNames = fst <$> sqlColumns @a
