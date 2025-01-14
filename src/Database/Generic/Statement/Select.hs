module Database.Generic.Statement.Select where

import Database.Generic.Entity (Entity)
import Database.Generic.Entity qualified as Entity
import Database.Generic.Entity.SqlTypes (SqlValue(..))
import Database.Generic.Entity.ToSql (ToSqlValue)
import Database.Generic.Statement.Fields (ReturningFields(..), fieldNames)
import Database.Generic.Statement.Where (Where(..), Whereable(..), idEquals)
import Database.Generic.Statement.Returning (NowReturning, Returning)
import Database.Generic.Prelude
import Database.Generic.Serialize (Serialize(..))
import Database.Generic.Table (TableName)
import Witch qualified as W

data Columns = All | Some ![String]

data OneOrMany = One | Many

instance Serialize Columns db where
  serialize All       = "*"
  serialize (Some cs) = intercalate ", " cs

-- | Select one or many values of type 'a', but only fields 'fs'.
data Select (o :: OneOrMany) fs a = Select
  { columns :: !Columns
  , from    :: !TableName
  , where'  :: !(Where a)
  }

type instance Returning (Select o fs _) = fs

type instance NowReturning (Select o a a) fs = Select o fs a

instance ReturningFields (Select o a a) where
  fields s p = Select
    { columns = Some $ fieldNames p
    , from    = s.from
    , where'  = s.where'
    }

instance Serialize SqlValue db => Serialize (Select o fs a) db where
  serialize s = unwords
    [ "SELECT", serialize s.columns
    , "FROM", serialize s.from
    , "WHERE", serialize @_ @db s.where'
    , ";"
    ]

selectById :: forall a f b.
  (Entity f a, HasField f a b, ToSqlValue b) => b -> Select One a a
selectById b = Select
  { columns = All
  , from    = Entity.tableName @_ @a
  , where'  = W.from $ idEquals @a b
  }

instance Whereable (Select o fs a) a where
  where' s w = s { where' = s.where' `And` W.from w }
