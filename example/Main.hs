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
import Data.Pool (Pool)
import Data.Pool qualified as Pool
import Database.Generic.Database (PostgreSQL)
import Database.Generic.Entity (Entity(..))
import Database.Generic.Entity.ToSql (toSqlValue)
import Database.Generic.Class (MonadDb(..), MonadDbWithConn(..))
import Database.Generic.Operations qualified as Db
import Database.Generic.Statement qualified as Statement
import Database.HDBC qualified as HDBC
import Database.HDBC.PostgreSQL qualified as PSQL
import Database.PostgreSQL.Simple.Options as PSQL
import GHC.Generics (Generic)

data Person = Person { name :: String, age :: Int }
  deriving (Entity "name", Generic, Show)

type Env = Pool PSQL.Connection

mkEnv :: String -> Int -> String -> String -> String -> IO Env
mkEnv host port dbname user password = do
  let connStr = BS.unpack $ PSQL.toConnectionString $ PSQL.defaultOptions
        { host     = pure host
        , port     = pure port
        , dbname   = pure dbname
        , user     = pure user
        , password = pure password
        }
  Pool.newPool $ Pool.defaultPoolConfig
    (PSQL.connectPostgreSQL connStr) HDBC.disconnect 10 10

newtype AppM a = AppM (ReaderT Env IO a)
  deriving newtype (Applicative, Functor, Monad, MonadIO, MonadReader Env)

type Db = PostgreSQL

newtype DbError = DbError String deriving Show

instance Exception DbError

instance MonadDb AppM Identity PSQL.Connection where
  type Error AppM Identity = DbError
  execute conn (Identity statement) = fmap (Identity . Right) do
    liftIO $ HDBC.runRaw conn (Statement.serialize @_ @Db statement)

instance MonadDbWithConn AppM PSQL.Connection where
  withConn f = ask >>= \env -> liftIO $ Pool.withResource env $ runAppM env . f

runAppM :: Env -> AppM a -> IO a
runAppM e (AppM m) = runReaderT m e

main :: IO ()
main = do
  let p = Person "John" 21
  env <- mkEnv "127.0.0.1" 5432 "postgres" "demo" "demo"
  _ <- runAppM env $ Db.tx do
    x <- Db.execute $ Statement.createTable @Person True
    x <- Db.execute $ Statement.createTable @Person True
    liftIO $ print x
    liftIO $ print "ran AppM"
    pure $ Right 6
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
