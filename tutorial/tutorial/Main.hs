{-# LANGUAGE BlockArguments       #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE OverloadedRecordDot  #-}

module Main where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader(..), ReaderT(..))
import Data.ByteString.Char8 qualified as BS
import Data.Functor.Identity (Identity(..))
import Data.Int (Int64)
import Database.Generic
import Database.Generic.Database (PostgreSQL)
import Database.Generic.Entity.SqlTypes (DbValue)
import Database.Generic.Prelude (debug')
import Database.Generic.Serialize (serialize)
import Database.Generic.Server qualified as Server
import Database.Generic.Statement.Output (Output(..), OutputType(..))
import Database.HDBC qualified as HDBC
import Database.HDBC.PostgreSQL qualified as PSQL
import Database.PostgreSQL.Simple.Options as PSQL
import GHC.Generics (Generic)
import Witch (from)

-- | Data type we want to persist.
data Person = Person { age :: !Int64, name :: !String }
  deriving (Generic, PrimaryKey "name", Show)

-- | Connection string to access our PostgreSQL DB.
type ConnStr = String

-- | Construct a connection string.
connStr :: String -> Int -> String -> String -> String -> ConnStr
connStr host port dbname user password =
  BS.unpack $ PSQL.toConnectionString $ PSQL.defaultOptions
    { host     = pure host
    , port     = pure port
    , dbname   = pure dbname
    , user     = pure user
    , password = pure password
    }

-- | Our application monad. Common ReaderT IO pattern.
newtype AppM a = AppM (ReaderT ConnStr IO a)
  deriving newtype (Applicative, Functor, Monad, MonadIO, MonadReader ConnStr)

runAppM :: ConnStr -> AppM a -> IO a
runAppM e (AppM m) = runReaderT m e

-- | Enable our application to communicate with PostgreSQL.
instance MonadDb AppM Identity PSQL.Connection DbValue where
  executeStatement conn (Identity (s, o)) = Identity . Right <$> liftIO do
    let serialized = debug' "Serialized statement" $ serialize @_ @PostgreSQL s
    case debug' "Expected output type" o of
      OutputTypeAffected -> OutputAffected . debug' "OutputAffected" <$> HDBC.run conn serialized []
      OutputTypeNada     -> debug' "OutputNada" OutputNada <$ HDBC.runRaw conn serialized
      OutputTypeRows     -> OutputRows . debug' "OutputRows" .
        fmap (fmap from) <$> HDBC.quickQuery' conn serialized []

-- | Enable our application to create new connections to PostgreSQL.
instance MonadDbNewConn AppM PSQL.Connection where
  newDbConn = liftIO . PSQL.connectPostgreSQL =<< ask

main :: IO ()
main = do
  let c    = connStr "127.0.0.1" 5432 "postgres" "demo" "demo"
  let john = Person 21 "John"
  let info m s = do
        putStrLn $ "\n" <> m
        print =<< runAppM c (tx $ execute s)

  info "Create table if not exists" $ createTable @Person @_ True

  info "Delete All" $ returning $ deleteAll @Person

  info "Delete by ID" $ deleteById @Person "John"

  info "Insert one" $ returning $ insertOne $ john{age=55}

  info "Insert two, returning age" $
    insertMany [john{age=25, name="Bob"}, john {name = "Mary"}] ==> field @"age"

  info "Select by ID" $ selectById @Person john.name

  info "Select all" $ selectAll @Person

  info "Select all, select 1 fields" $
    selectAll @Person ==> field2 @"age" @"name"

  info "Select all, order by age" $ orderBy (field @"age") $ selectAll @Person

  info "Select all, limit 1" $
    limit 1 $ orderBy (field @"name") $ selectAll @Person

  info "Select all, limit 1, offset 2" $
    limitOffset 1 2 $ orderBy (field @"name") $ selectAll @Person

  info "Select specific fields by ID" $
    selectById @Person john.name ==> field @"age"

  putStrLn "\nStarting a server which will proxy any statements"
  Server.run (runAppM c) 1234 Server.developmentCors
