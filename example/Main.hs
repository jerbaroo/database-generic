{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies       #-}

module Main where

import Control.Exception (Exception)
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString.Char8 qualified as BS
import Data.Functor.Identity (Identity(..))
import Database.Generic.Database (PostgreSQL)
import Database.Generic.Entity (Entity(..))
import Database.Generic.Entity.ToSql (toSqlValue)
import Database.Generic.Class (MonadDb(..), MonadDbNewConn(..))
import Database.Generic.Operations qualified as Db
import Database.Generic.Serialize (serialize)
import Database.Generic.Statement qualified as Statement
import Database.HDBC qualified as HDBC
import Database.HDBC.PostgreSQL qualified as PSQL
import Database.PostgreSQL.Simple.Options as PSQL
import GHC.Generics (Generic)

data Person = Person { name :: String, age :: Int }
  deriving (Entity "name", Generic, Show)

type Env = String

env :: String -> Int -> String -> String -> String -> Env
env host port dbname user password =
  BS.unpack $ PSQL.toConnectionString $ PSQL.defaultOptions
    { host     = pure host
    , port     = pure port
    , dbname   = pure dbname
    , user     = pure user
    , password = pure password
    }

newtype AppM a = AppM (ReaderT Env IO a)
  deriving newtype (Applicative, Functor, Monad, MonadIO, MonadReader Env)

type Db = PostgreSQL

newtype DbError = DbError String deriving Show

instance Exception DbError

instance MonadDb AppM Identity PSQL.Connection where
  type Error AppM Identity = DbError
  execute conn (Identity s) = fmap (Identity . Right) do
    liftIO $ HDBC.runRaw conn $ serialize @_ @Db s

instance MonadDbNewConn AppM PSQL.Connection where
  newConn = liftIO . PSQL.connectPostgreSQL =<< ask

runAppM :: Env -> AppM a -> IO a
runAppM e (AppM m) = runReaderT m e

main :: IO ()
main = do
  let p    = Person "John" 21
  let env' = env "127.0.0.1" 5432 "postgres" "demo" "demo"
  _ <- runAppM env' $ Db.executeTx $ Statement.createTable @Person True
  _ <- runAppM env' $ Db.tx do
    x <- Db.execute $ Statement.createTable @Person True
    x <- Db.execute $ Statement.createTable @Person True
    liftIO $ print x
    liftIO $ print "ran AppM"
    pure $ Right 6
  _ <- runAppM env' $ Db.tx $ Db.execute $ Statement.createTable @Person True
  f <- runAppM env' $ Db.tx $ Db.execute $ Statement.deleteById @Person "John"
  print f
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
