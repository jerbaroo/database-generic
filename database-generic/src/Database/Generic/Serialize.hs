{-# LANGUAGE UndecidableInstances #-}

module Database.Generic.Serialize where

import Database.Generic.Database (PostgreSQL, SQLite)
import Database.Generic.Entity.SqlTypes (DbT(..), DbType, DbValue, Unit(..))
import Database.Generic.Prelude

-- TODO alter param order
class Serialize a db where
  serialize :: a -> String

instance Serialize DbType PostgreSQL where
  serialize (DbInt64  Unit) = "BIGINT"
  serialize (DbString Unit) = "VARCHAR"

instance Serialize DbType SQLite where
  serialize = serialize @_ @PostgreSQL

instance Serialize DbValue PostgreSQL where
  serialize (DbInt64 i)  = show i
  serialize (DbString s) = "'" <> s <> "'"
  -- serialize s = "Serialize SqlValue db: Not implemented for " <> show s

instance Serialize DbValue SQLite where
  serialize = serialize @_ @PostgreSQL

parens :: [String] -> String
parens xs = "(" <> intercalate ", " xs <> ")"

statement :: String -> String
statement = (<> ";")
