{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeFamilies        #-}

module Main where

import Control.Monad.Reader (MonadReader(..), ReaderT(..))
import Control.Monad.IO.Class (MonadIO(..))
import Data.ByteString.Char8 qualified as BS
import Data.Functor.Identity (Identity(..))
import Data.Int (Int64)
import Database.Generic
import Database.Generic.Database (PostgreSQL)
import Database.Generic.Entity qualified as Db
import Database.Generic.Entity.FromSql (fromSqlValues)
import Database.Generic.Entity.ToSql (sqlColumnNames, sqlColumnTypes, toSqlValue, toSqlValues)
import Database.Generic.Field (field)
import Database.Generic.Prelude (debug)
import Database.Generic.Serialize (serialize)
import Database.Generic.Statement.Output (Output(..), OutputType(..), outputType)
import Database.HDBC qualified as HDBC
import Database.HDBC.PostgreSQL qualified as PSQL
import Database.PostgreSQL.Simple.Options as PSQL
import GHC.Generics (Generic)

-- | Data type we want to store in our database.
data Person = Person { name :: String, age :: Int64 }
  deriving (Entity "name", Generic, Show)

-- | Connection string to access DB.
type Env = String

-- | Construct a connection string.
env :: String -> Int -> String -> String -> String -> Env
env host port dbname user password =
  BS.unpack $ PSQL.toConnectionString $ PSQL.defaultOptions
    { host     = pure host
    , port     = pure port
    , dbname   = pure dbname
    , user     = pure user
    , password = pure password
    }

-- | Our application monad.
newtype AppM a = AppM (ReaderT Env IO a)
  deriving newtype (Applicative, Functor, Monad, MonadIO, MonadReader Env)

runAppM :: Env -> AppM a -> IO a
runAppM e (AppM m) = runReaderT m e

-- | Enable 'AppM' to communicate with PostgreSQL.
instance MonadDb AppM Identity PSQL.Connection where
  executeStatement conn (Identity (s :: s)) = fmap (Identity . Right) $ liftIO do -- TODO error handling.
    let x = debug $ serialize @_ @PostgreSQL s
    case outputType @s of
      OutputTypeAffected -> OutputAffected . debug <$> HDBC.run conn x []
      OutputTypeNada     -> OutputNada <$  HDBC.runRaw conn x
      OutputTypeRows     -> OutputRows <$> HDBC.quickQuery' conn x []

-- | Enable 'AppM' to connect to PostgreSQL.
instance MonadDbNewConn AppM PSQL.Connection where
  newDbConn = liftIO . PSQL.connectPostgreSQL =<< ask

main :: IO ()
main = do
  let e = env "127.0.0.1" 5432 "postgres" "demo" "demo"
  -- Create table if not exists, twice.
  runAppM e $ tx_ do
    liftIO . print =<< execute (createTable @Person True)
    liftIO . print =<< execute (createTable @Person True)
    pure $ Right ()
  -- Delete by ID.
  _ <- runAppM e $ tx $ execute $ deleteById @Person "John"
  let john = Person "John" 21
  -- Insert one.
  print =<< runAppM e (executeTx $ insertOne $ john{age=55 })
  -- Insert two.
  print =<< runAppM e do
    executeTx $ insertMany [john{name="Foo"}, john {name = "Mary"}]
  -- Select by ID.
  print =<< runAppM e (executeTx $ selectById @Person john.name)
  -- Select specific fields by ID.
  runAppM e $ tx_ do
    liftIO . print =<< execute do
      selectById @Person john.name ==> field @"age" @Person
    pure $ Right ()
  print $ Db.primaryKeyFieldName @Person
  print $ Db.primaryKey john
  print $ toSqlValue $ Db.primaryKey john
  print $ sqlColumnNames @Person
  print $ sqlColumnTypes @Person
  print $ toSqlValues john
  print @Person $ fromSqlValues $ toSqlValues john
