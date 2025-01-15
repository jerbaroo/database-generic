module Database.Generic.Entity.FieldName where

import Generics.Eot qualified as G
import Database.Generic.Prelude

-- | Name of a field. For example: "foo" in 'data X = Y { foo :: Int }'.
newtype FieldName = FieldName String deriving Eq

instance From FieldName String

-- | Types that represent a named-field.
class HasFieldName f where
  fieldName :: FieldName

instance {-# OVERLAPPABLE #-} Typeable f => HasFieldName f where
  fieldName = FieldName $ showType' @f

class HasFieldNames a where
  fieldNames :: [FieldName]

data HasFieldNamesError
  = MoreThanOneConstructor !String
  | NoConstructors         !String
  | NoFields               !String
  | NoSelectors            !String
  deriving Show

instance Exception HasFieldNamesError

instance (G.HasEot a, Typeable a) => HasFieldNames a where
  fieldNames =
    case G.constructors $ G.datatype $ Proxy @a of
      []  -> throw $ NoConstructors $ showType @a
      [c] -> case G.fields c of
        G.Selectors   fields -> FieldName <$> fields
        G.NoSelectors _      -> throw $ NoSelectors $ showType @a
        G.NoFields           -> throw $ NoFields $ showType @a
      _ -> throw $ MoreThanOneConstructor $ showType @a
