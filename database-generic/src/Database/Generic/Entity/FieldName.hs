module Database.Generic.Entity.FieldName where

import Data.Aeson qualified as Aeson
import Generics.Eot qualified as G
import Database.Generic.Prelude

-- | Name of a field as a string.
--
-- For example: "foo" in 'data X = Y { foo :: Int }'.
newtype FieldName = FieldName String
  deriving (Eq, Generic, Show)

instance Aeson.FromJSON FieldName

instance From FieldName String

instance IsString FieldName where
  fromString = FieldName

-- | 'f' is the name of a field, which will be used as SQL column name.
--
-- Currently we defer this functionality to 'Typeable', but will we always?
type HasFieldName = Typeable

fieldName :: forall f. HasFieldName f => FieldName
fieldName = FieldName $ showType' @f

-- TODO Is possible for 'fieldName' and 'fieldNames' to be inconsistent?
class HasFieldNames a where
  fieldNames :: [FieldName]

data HasFieldNamesError
  = MoreThanOneConstructor !String
  | NoConstructors         !String
  | NoFields               !String
  | NoSelectors            !String
  deriving Show

instance Exception HasFieldNamesError

instance (G.HasEot a, HasFieldName a) => HasFieldNames a where
  fieldNames =
    case G.constructors $ G.datatype $ Proxy @a of
      []  -> throw $ NoConstructors $ showType @a
      [c] -> case G.fields c of
        G.Selectors   fields -> FieldName . showTypeT <$> fields
        G.NoSelectors _      -> throw $ NoSelectors $ showType @a
        G.NoFields           -> throw $ NoFields $ showType @a
      _ -> throw $ MoreThanOneConstructor $ showType @a
