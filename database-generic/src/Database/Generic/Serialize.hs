module Database.Generic.Serialize where

import Database.Generic.Database (PostgreSQL, SQLite)
import Database.Generic.Entity.SqlTypes (SqlType(..), SqlValue(..))
import Database.Generic.Prelude

-- TODO alter param order
class Serialize a db where
  serialize :: a -> String

instance Serialize SqlType PostgreSQL where
  serialize SqlVarChar = "VARCHAR"
  serialize SqlBigInt  = "BIGINT"

instance Serialize SqlType SQLite where
  serialize = serialize @_ @PostgreSQL

instance Serialize SqlValue PostgreSQL where
  serialize (SqlString s) = "'" <> s <> "'"
  serialize (SqlInt64 i)  = show i
  serialize s = "Serialize SqlValue db: Not implemented for " <> show s

instance Serialize SqlValue SQLite where
  serialize = serialize @_ @PostgreSQL

parens :: [String] -> String
parens xs = "(" <> intercalate ", " xs <> ")"

statement :: String -> String
statement = (<> ";")
