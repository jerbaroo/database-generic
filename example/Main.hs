{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies       #-}

module Main where

import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString.Char8 qualified as BS
import Data.Functor.Identity (Identity(..))
import Database.Generic.Database (PostgreSQL)
import Database.Generic.Entity (Entity(..))
import Database.Generic.Entity.ToSql (toSqlValue)
import Database.Generic.Class (MonadDb(..), MonadDbNewConn(..))
import Database.Generic.Operations qualified as Db
import Database.Generic.Prelude
import Database.Generic.Serialize (serialize)
import Database.Generic.Statement qualified as Statement
import Database.HDBC qualified as HDBC
import Database.HDBC.PostgreSQL qualified as PSQL
import Database.PostgreSQL.Simple.Options as PSQL
import GHC.Generics (Generic)

data Person = Person { name :: String, age :: Int }
  deriving (Entity "name", Generic, Show)

type Env = String -- Connection string to access DB.

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

instance MonadDb AppM Identity PSQL.Connection where
  execute conn (Identity s) = fmap (Identity . Right) do -- Ignoring errors.
    liftIO $ HDBC.runRaw conn $ debug $ serialize @_ @PostgreSQL s

instance MonadDbNewConn AppM PSQL.Connection where
  newConn = liftIO . PSQL.connectPostgreSQL =<< ask

runAppM :: Env -> AppM a -> IO a
runAppM e (AppM m) = runReaderT m e

main :: IO ()
main = do
  let env' = env "127.0.0.1" 5432 "postgres" "demo" "demo"
  pure ()
  -- _ <- runAppM env' $ Db.executeTx $ Statement.createTable @Person True
  -- _ <- runAppM env' $ Db.tx do
  --   x <- Db.execute $ Statement.createTable @Person True
  --   x <- Db.execute $ Statement.createTable @Person True
  --   liftIO $ print x
  --   liftIO $ print "ran AppM"
  --   pure $ Right 6
  -- _ <- runAppM env' $ Db.tx $ Db.execute $ Statement.createTable @Person True
  -- f <- runAppM env' $ Db.tx $ Db.execute $ Statement.deleteById @Person "John"
  -- let john = Person "John" 21
  -- f <- runAppM env' $ Db.tx $ Db.execute $ Statement.upsert john
  -- print f
  -- print john
  -- print $ primaryKeyFieldName @_ @Person
  -- print $ primaryKey john
  -- print $ toSqlValue $ primaryKey john
  -- print $ sqlFieldNames @_ @Person
  -- print $ sqlFieldTypes @_ @Person
  -- let asSql = toSqlValues john
  -- print asSql
  -- let john' = fromSqlValues asSql
  -- print @Person john'
