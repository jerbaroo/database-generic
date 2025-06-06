module Database.Generic.Entity.EntityName where

import Database.Generic.Prelude

newtype EntityName = EntityName String deriving (Eq, Show)

instance From EntityName String

instance IsString EntityName where
  fromString = EntityName
