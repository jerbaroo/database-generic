module Database.Generic.Statement.Where where

import Database.Generic.Entity (Entity)
import Database.Generic.Entity qualified as Entity
import Database.Generic.Entity.SqlTypes (SqlValue(..))
import Database.Generic.Entity.ToSql (ToSqlValue(..))
import Database.Generic.Prelude
import Database.Generic.Serialize (Serialize(..))

newtype Where = Equals (String, SqlValue)

idEquals :: forall a f b.
  (Entity f a, HasField f a b, ToSqlValue b) => b -> Where
idEquals b = Equals (Entity.primaryKeyFieldName @a, toSqlValue b)

instance Serialize SqlValue db => Serialize Where db where
  serialize (Equals (column, value)) =
    column <> " = " <> serialize @_ @db value

newtype Wheres = Wheres [Where]

instance Serialize SqlValue db => Serialize Wheres db where
  serialize (Wheres ws) = intercalate " AND " $ serialize @_ @db <$> ws
