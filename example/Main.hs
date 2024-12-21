{-# LANGUAGE DataKinds #-}

module Main where

import Database.Generic.Entity (Entity(..))
import Database.Generic.Entity.ToSql (toSqlValue)
import GHC.Generics (Generic)

main :: IO ()
main = do
  let p = Person "John" 21
  print p
  print $ primaryKeyFieldName @Person
  print $ primaryKey p
  print $ toSqlValue $ primaryKey p
  let asSql = entityToSql p
  print asSql
  let p' = entityFromSql $ snd <$> asSql
  print @Person p'

data Person = Person { name :: String, age :: Int } deriving (Generic, Show)

instance Entity Person "name" where
