module Database.Generic.Statement.Order where

import Data.Aeson qualified as Aeson
import Database.Generic.Prelude
import Database.Generic.Serialize (Serialize(..))

data Order = Asc | Desc deriving (Eq, Generic, Show)

instance Aeson.FromJSON Order

instance Serialize Order db where
  serialize Asc  = "ASC"
  serialize Desc = "DESC"
