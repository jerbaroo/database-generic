module Database.Generic.Entity.EntityName where

import Database.Generic.Prelude

newtype EntityName = EntityName String

instance From EntityName String
