{-# LANGUAGE UndecidableInstances #-}

module Database.Generic.Entity.DbColumns where

import Database.Generic.Entity.DbTypes (HasDbType(dbType), DbTypeN)
import Database.Generic.Entity.FieldName (FieldName(..), FieldNameTransformation(..))
import Database.Generic.Prelude
import Generics.Eot qualified as G

-- | Name of column and database column type for each field of 'a'.
class HasDbColumns a where
  sqlColumns :: [(FieldName, DbTypeN)]

instance (G.HasEot a, GHasDbColumns a G.Datatype (G.Eot a))
  => HasDbColumns a where
  sqlColumns = gDbColumns @a @_ @(G.Eot a) $ G.datatype $ Proxy @a

class GHasDbColumns a meta ga where
  gDbColumns :: meta -> [(FieldName, DbTypeN)]

instance GHasDbColumns a [String] fields
  => GHasDbColumns a G.Datatype (Either fields G.Void) where
  gDbColumns datatype =
    case datatype of
      G.Datatype _ [G.Constructor _ (G.Selectors fields)] ->
        gDbColumns @a @_ @fields fields
      G.Datatype name [G.Constructor cName (G.NoSelectors _)] ->
        error $ name <> " constructor " <> cName <> " has no selectors"
      G.Datatype name _ -> error $ name <> " must have exactly one constructor"

instance (HasDbType f, GHasDbColumns a [String] fs)
  => GHasDbColumns a [String] (f, fs) where
  gDbColumns []     = error "impossible"
  gDbColumns (f:fs) =
    (fieldNameT @a $ FieldName f, dbType @f) : gDbColumns @a @_ @fs fs

instance GHasDbColumns a [String] () where
  gDbColumns [] = []
  gDbColumns _  = error "impossible"

fieldNames :: forall a. HasDbColumns a => [FieldName]
fieldNames = fst <$> sqlColumns @a
