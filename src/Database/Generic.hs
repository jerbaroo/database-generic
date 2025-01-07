module Database.Generic (module Database.Generic, module X) where

import Database.Generic.Entity (Entity, EntityP)
import Database.Generic.Statement (Statement, statement)
import Database.Generic.Statement.CreateTable qualified as CreateTable
import Database.Generic.Statement.CreateTable as X (createTable)
import Database.Generic.Statement.Delete qualified as Delete
import Database.Generic.Statement.Delete as X (deleteById)
import Database.Generic.Statement.Insert qualified as Insert
import Database.Generic.Statement.Insert as X (insertMany, insertOne)
import Database.Generic.Statement.Select qualified as Select
import Database.Generic.Statement.Select as X (selectById)
import Database.Generic.Statement.Returning (StatementType(..))
import Database.Generic.Prelude

createTable' :: forall a f b. EntityP f a b => Bool -> Statement '[Nada]
createTable' = statement . CreateTable.createTable @a

deleteById' :: forall a f b. EntityP f a b => b -> Statement '[OneAffected a]
deleteById' = statement . Delete.deleteById

insertOne' :: forall a f. Entity f a => a -> Statement '[OneAffected a]
insertOne' = statement . Insert.insertOne

insertMany' :: forall a f. Entity f a => [a] -> Statement '[ManyAffected a]
insertMany' = statement . Insert.insertMany

selectById' :: forall a f b. EntityP f a b => b -> Statement '[MaybeOne a]
selectById' = statement . Select.selectById
