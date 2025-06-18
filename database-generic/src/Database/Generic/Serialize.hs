{-# LANGUAGE UndecidableInstances #-}

module Database.Generic.Serialize where

import Database.Generic.Database (PostgreSQL)
import Database.Generic.Entity.DbTypes (DbT(..), DbType, DbTypeN(..), DbValue, DbValueN, Unit(..))
import Database.Generic.Prelude

-- TODO alter param order
class Serialize a db where
  serialize :: a -> String

instance Serialize DbType PostgreSQL where
  serialize (DbBool    Unit) = "BOOLEAN"
  serialize (DbBytes   Unit) = "BINARY"
  serialize (DbInt64   Unit) = "BIGINT"
  serialize (DbInteger Unit) = "BIGINT"
  serialize (DbString  Unit) = "VARCHAR"

instance Serialize DbTypeN PostgreSQL where
  serialize (DbTypeN False dbType) = serialize @_ @PostgreSQL dbType <> " NOT NULL"
  serialize (DbTypeN True  dbType) = serialize @_ @PostgreSQL dbType

instance Serialize DbValue PostgreSQL where
  serialize (DbBool    b)  = show b
  serialize (DbBytes   b)  = show b
  serialize (DbInt64   i)  = show i
  serialize (DbInteger i)  = show i
  serialize (DbString  s) = "'" <> s <> "'"

instance Serialize DbValueN PostgreSQL where
  serialize Nothing  = "NULL"
  serialize (Just a) = serialize @_ @PostgreSQL a

parens :: [String] -> String
parens xs = "(" <> intercalate ", " xs <> ")"

statement :: String -> String
statement = (<> ";")
