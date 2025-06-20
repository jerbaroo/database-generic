module Database.Generic.Prelude (debug, debug', showType, showType', showTypeT, module X) where

import Control.Arrow as X ((&&&), (***))
import Control.Comonad as X (Comonad(extract))
import Control.Exception as X (Exception(displayException), SomeException, throw, toException)
import Control.FromSum as X (fromEither, fromEitherM)
import Control.Monad.Trans.Class as X (lift)
import Control.Monad.Trans.Except as X (ExceptT(ExceptT), runExceptT)
import Data.Convertible as X (Convertible(safeConvert), convert)
import Data.Char as X (toLower)
import Data.Either as X (Either(Left, Right), fromLeft, fromRight)
import Data.Either.Extra as X (mapLeft)
import Data.Functor as X ((<&>))
import Data.Functor.Identity as X (Identity(Identity))
import Data.Int as X (Int64)
import Data.Kind as X (Type)
import Data.List as X (intercalate)
import Data.List.Extra as X (replace)
import Data.List.NonEmpty as X (NonEmpty((:|)))
import Data.Maybe as X (Maybe(Just, Nothing), catMaybes, fromMaybe, maybe)
import Data.Proxy as X (Proxy(Proxy))
import Data.Semigroup as X (Semigroup((<>)))
import Data.String as X (IsString(fromString))
import Data.Type.Equality as X (type (~))
import Data.Typeable as X (Typeable, typeRep)
import Debug.Trace as X (trace)
import GHC.Generics as X (Generic)
import GHC.Records as X (HasField(getField))
import Prelude as X (Applicative(pure), Bool(True, False), Eq, Functor(fmap), IO, Int, Integer, Monad((>>=)), Monoid, Read, Show(show), String, (.), ($), (==), (/=), (>>=), (<$>), const, error, flip, fst, undefined, unwords, zip)
import Witch as X (From(from), TryFrom(tryFrom), into)
import Witch.Utility as X (over)

debug :: Show a => a -> a
debug a = trace (show a) a

debug' :: Show a => String -> a -> a
debug' s a = trace (s <> ": " <> show a) a

-- | Type of 'a' as a 'String'.
showType :: forall a. Typeable a => String
showType = show $ typeRep $ Proxy @a

-- | Like 'showType' but with 'showTypeT' transformation applied.
showType' :: forall a. Typeable a => String
showType' = showTypeT $ showType @a

-- | Transformation used in 'showType''.
showTypeT :: String -> String
showTypeT = replace "\"" "" . fmap toLower
