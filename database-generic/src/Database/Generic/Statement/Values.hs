module Database.Generic.Statement.Values where

import Database.Generic.Entity.SqlTypes (SqlValue(..))
import Database.Generic.Prelude
import Database.Generic.Serialize (Serialize(..))

newtype Values = Values [SqlValue] deriving (Eq, Show)

instance Serialize SqlValue db => Serialize Values db where
  serialize (Values vs) = "(" <> intercalate "," (serialize @_ @db <$> vs) <> ")"
