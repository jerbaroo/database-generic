module Database.Generic.Prelude (debug, module X) where

import Control.Comonad as X (Comonad(extract))
import Control.Exception as X (Exception, SomeException, throw)
import Data.Convertible as X (Convertible(safeConvert), convert)
import Data.Char as X (toLower)
import Data.Either as X (Either(Left, Right))
import Data.Functor as X ((<&>))
import Data.Functor.Identity as X (Identity(Identity))
import Data.Kind as X (Type)
import Data.List as X (intercalate)
import Data.List.Extra as X (replace)
import Data.List.NonEmpty as X (NonEmpty((:|)))
import Data.Maybe as X (Maybe(Just, Nothing), fromMaybe)
import Data.Proxy as X (Proxy(Proxy))
import Data.Semigroup as X (Semigroup((<>)))
import Data.Typeable as X (Typeable, typeRep)
import Debug.Trace as X (trace)
import GHC.Records as X (HasField(getField))
import Prelude as X (Applicative(pure), Bool, Functor(fmap), Int, Monad((>>=)), Monoid, Show(show), String, (.), ($), (==), (/=), (>>=), (<$>), const, flip, error, undefined, unwords, zip)

debug :: Show a => a -> a
debug a = trace (show a) a
