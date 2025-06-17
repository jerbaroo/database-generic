{-# LANGUAGE UndecidableInstances #-}

module Database.Generic.Statement.Values where

import Data.Aeson qualified as Aeson
import Database.Generic.Entity.DbTypes (DbValueN)
import Database.Generic.Prelude
import Database.Generic.Serialize (Serialize(..))

newtype Values = Values [DbValueN]
  deriving (Eq, Generic, Show)

instance Aeson.FromJSON Values

instance Serialize DbValueN db => Serialize Values db where
  serialize (Values vs) = "(" <> intercalate "," (serialize @_ @db <$> vs) <> ")"
