{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main where

import Control.Monad.Reader (ReaderT, runReaderT)
import Database.Generic.Entity (Entity(..))
import Database.Generic.Entity.ToSql (toSqlValue)
import GHC.Generics (Generic)

data Person = Person { name :: String, age :: Int }
  deriving (Entity "name", Generic, Show)

type Env    = Int
type AppM a = ReaderT Env IO a

runAppM :: Env -> AppM a -> IO a
runAppM = flip runReaderT

main :: IO ()
main = do
  let p = Person "John" 21
  print p
  print $ primaryKeyFieldName @_ @Person
  print $ primaryKey p
  print $ toSqlValue $ primaryKey p
  print $ sqlFieldNames @_ @Person
  print $ sqlFieldTypes @_ @Person
  let asSql = toSqlValues p
  print asSql
  let p' = fromSqlValues asSql
  print @Person p'
