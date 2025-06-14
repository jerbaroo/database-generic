{-# LANGUAGE UndecidableInstances #-}

module Database.Generic.Entity.SqlColumns where

import Database.Generic.Entity.SqlTypes (HasDbType(dbType), DbType)
import Database.Generic.Entity.FieldName (FieldName(..), FieldNameTransformation(..))
import Database.Generic.Prelude
import Generics.Eot qualified as G

-- | Name of column and database column type for each field of 'a'.
class HasDbColumns dbt a where
  sqlColumns :: [(FieldName, dbt)]

instance (G.HasEot a, GHasDbColumns dbt a G.Datatype (G.Eot a))
  => HasDbColumns dbt a where
  sqlColumns = gDbColumns @dbt @a @_ @(G.Eot a) $ G.datatype $ Proxy @a

class GHasDbColumns dbt a meta ga where
  gDbColumns :: meta -> [(FieldName, dbt)]

instance GHasDbColumns dbt a [String] fields
  => GHasDbColumns dbt a G.Datatype (Either fields G.Void) where
  gDbColumns datatype =
    case datatype of
      G.Datatype _ [G.Constructor _ (G.Selectors fields)] ->
        gDbColumns @dbt @a @_ @fields fields
      G.Datatype name [G.Constructor cName (G.NoSelectors _)] ->
        error $ name <> " constructor " <> cName <> " has no selectors"
      G.Datatype name _ -> error $ name <> " must have exactly one constructor"

instance (HasDbType dbt f, GHasDbColumns dbt a [String] fs)
  => GHasDbColumns dbt a [String] (f, fs) where
  gDbColumns []     = error "impossible"
  gDbColumns (f:fs) =
    (fieldNameT $ FieldName f, dbType @dbt @f)
      : gDbColumns @dbt @a @_ @fs fs

instance GHasDbColumns dbt a [String] () where
  gDbColumns [] = []
  gDbColumns _  = error "impossible"

fieldNames :: forall dbt a. HasDbColumns dbt a => [FieldName]
fieldNames = fst <$> sqlColumns @dbt @a
