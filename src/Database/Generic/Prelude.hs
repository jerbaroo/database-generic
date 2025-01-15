module Database.Generic.Prelude (debug, showType, showType', module X) where

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
import Data.Typeable as X (Typeable, typeRep)
import Debug.Trace as X (trace)
import GHC.Records as X (HasField(getField))
import Prelude as X (Applicative(pure), Bool(True, False), Eq, Functor(fmap), Int, Integer, Monad((>>=)), Monoid, Show(show), String, (.), ($), (==), (/=), (>>=), (<$>), const, error, flip, fst, undefined, unwords, zip)
import Witch as X (Utf8S, From(from), into, unsafeFrom, via)

debug :: Show a => a -> a
debug a = trace (show a) a

-- | Type of 'a' as a 'String'.
showType :: forall a. Typeable a => String
showType = show $ typeRep $ Proxy @a

-- | Like 'showType' but with quotes removed.
showType' :: forall a. Typeable a => String
showType' = replace "\"" "" $ showType @a
