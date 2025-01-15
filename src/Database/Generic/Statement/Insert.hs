
module Database.Generic.Statement.Insert where

import Database.Generic.Entity (Entity)
import Database.Generic.Entity qualified as Entity
import Database.Generic.Entity.EntityName (EntityName)
import Database.Generic.Entity.FieldName (FieldName, HasFieldNames(..))
import Database.Generic.Entity.SqlTypes (SqlValue(..))
import Database.Generic.Entity.ToSql (toSqlValues)
import Database.Generic.Prelude
import Database.Generic.Serialize (Serialize(..))
import Database.Generic.Statement.Type.OneOrMany (OneOrMany(..))
import Database.Generic.Statement.Values (Values(..))
import Witch qualified as W

-- | Insert one or many values of type 'a'.
data Insert (o :: OneOrMany) a = Insert
  { table   :: !EntityName
  , columns :: ![FieldName]
  , rows    :: ![Values]
  }

instance Serialize SqlValue db => Serialize (Insert o a) db where
  serialize i = unwords
    [ "INSERT INTO", W.from i.table
    , "(", intercalate ", " $ from <$> i.columns, ") VALUES"
    , intercalate ", " $ serialize @_ @db <$> i.rows
    , ";"
    ]

insertOne :: forall a f. Entity f a => a -> Insert One a
insertOne a = Insert
  { table   = Entity.entityName @_ @a
  , columns = fieldNames @a
  , rows    = [Values $ toSqlValues a]
  }

insertMany :: forall a f. Entity f a => [a] -> Insert Many a
insertMany as = Insert
  { table   = Entity.entityName @_ @a
  , columns = fieldNames @a
  , rows    = Values . toSqlValues <$> as
  }
