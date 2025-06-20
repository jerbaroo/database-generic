module Database.Generic.Entity.Bytes (Utf8Bytes) where

import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Database.Generic.Prelude
import Witch as X (Utf8S, tryInto, via)
import Witch.Utility (unsafeFrom, withSource, withTarget)

-- | Wrapper over 'ByteString' to ensure safe conversion to/from UTF-8 'String'.
newtype Utf8Bytes = Utf8Bytes ByteString deriving (Eq, Show)

instance Aeson.FromJSON Utf8Bytes where
  parseJSON = fmap (from @String) <$> Aeson.parseJSON

instance Aeson.ToJSON Utf8Bytes where
  toJSON = Aeson.toJSON . into @String

instance From Utf8Bytes ByteString

instance From Utf8Bytes String where
  from = unsafeFrom @Utf8S . via @ByteString

instance From String Utf8Bytes where
  from = Utf8Bytes . from . into @Utf8S

instance TryFrom ByteString Utf8Bytes where
  tryFrom bs =
    mapLeft (withSource bs . withTarget @Utf8Bytes)
    $ fmap from
    $ tryInto @String -- Check if a valid UTF-8 encoded bytestring.
    $ into @Utf8S bs
