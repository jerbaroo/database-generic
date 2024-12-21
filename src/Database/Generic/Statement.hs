module Database.Generic.Statement where

import Database.Generic.Database (Database(..))
import Database.Generic.Entity (Entity)
import Database.Generic.Entity qualified as Entity
import Database.Generic.Prelude

createTable :: forall db a f. (Database db, Entity f a) => Bool -> String
createTable ifNotExists = intercalate " "
  [ "CREATE TABLE"
  , if ifNotExists then "IF NOT EXISTS" else ""
  , Entity.tableName @_ @a
  , intercalate "," $ columns @db @a
  , "("
  , ");"
  ]

-- | Description of each column for database 'db'.
-- Example: ["foo BIGINT PRIMARY KEY"]
columns :: forall db a f. (Database db, Entity f a) => [String]
columns = do
  let primaryName = Entity.primaryKeyFieldName @_ @a
  zip (Entity.sqlFieldNames @_ @a) (Entity.sqlFieldTypes @_ @a) <&>
    \(name, type') -> intercalate " "
      [ name
      , columnType @db type'
      , if name == primaryName then "PRIMARY KEY" else ""
      ]
