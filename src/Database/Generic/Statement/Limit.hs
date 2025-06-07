module Database.Generic.Statement.Limit where

import Database.Generic.Prelude

type Limit = Int

class Limitable s where
  limit :: Limit -> s -> s
