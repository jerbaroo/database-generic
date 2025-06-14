{-# LANGUAGE UndecidableInstances #-}

module Database.Generic.Statement.Values where

import Data.Aeson qualified as Aeson
import Database.Generic.Entity.SqlTypes (DbValue)
import Database.Generic.Prelude
import Database.Generic.Serialize (Serialize(..))

newtype Values = Values [DbValue]
  deriving (Eq, Generic, Show)

instance Aeson.FromJSON Values

instance Serialize DbValue db => Serialize Values db where
  serialize (Values vs) = "(" <> intercalate "," (serialize @_ @db <$> vs) <> ")"
