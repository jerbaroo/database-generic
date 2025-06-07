{-# LANGUAGE BlockArguments       #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE OverloadedRecordDot  #-}

module Main where

import Control.Monad.Reader (MonadReader(..), ReaderT(..))
import Control.Monad.IO.Class (MonadIO(..))
import Data.ByteString.Char8 qualified as BS
import Data.Functor.Identity (Identity(..))
import Data.Int (Int64)
import Database.Generic
import Database.Generic.Database (PostgreSQL)
import Database.Generic.Prelude (debug)
import Database.Generic.Serialize (serialize)
import Database.Generic.Statement.Output (Output(..), OutputType(..), outputType)
import Database.HDBC qualified as HDBC
import Database.HDBC.PostgreSQL qualified as PSQL
import Database.PostgreSQL.Simple.Options as PSQL
import GHC.Generics (Generic)

-- | Data type we want to persist.
data Person = Person { name :: !String, age :: !Int64 }
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
instance MonadDb AppM Identity PSQL.Connection where
  executeStatement conn (Identity (s :: s)) = fmap (Identity . Right) $ liftIO do -- TODO error handling.
    let x = debug $ serialize @_ @PostgreSQL s
    case debug $ outputType @s of
      OutputTypeAffected -> OutputAffected . debug <$> HDBC.run conn x []
      OutputTypeNada     -> debug OutputNada <$ HDBC.runRaw conn x
      OutputTypeRows     -> OutputRows . debug <$> HDBC.quickQuery' conn x []

-- | Enable our application to create new connections to PostgreSQL.
instance MonadDbNewConn AppM PSQL.Connection where
  newDbConn = liftIO . PSQL.connectPostgreSQL =<< ask

main :: IO ()
main = do
  let c    = connStr "127.0.0.1" 5432 "postgres" "demo" "demo"
  let john = Person "John" 21
  let info = putStrLn . ("\n" ++)
  info "Create table if not exists"
  runAppM c $ tx_ $ execute $ createTable @Person True
  info "Delete All"
  print =<< runAppM c (tx $ execute $ returning $ deleteAll @Person)
  info "Delete by ID"
  print =<< runAppM c (tx $ execute $ deleteById @Person "John")
  info "Insert one"
  print =<< runAppM c (tx $ execute $ returning $ insertOne $ john{age=55})
  info "Insert two"
  print =<< runAppM c (tx $ execute $ insertMany [john{name="Foo"}, john {name = "Mary"}] ==> field @"age")
  info "Select by ID"
  print =<< runAppM c (tx_ $ execute $ selectById @Person john.name)
  info "Select All"
  print =<< runAppM c (tx_ $ execute $ selectAll @Person)
  info "Select specific fields by ID"
  print =<< runAppM c (tx_ $ execute $
    selectById @Person john.name ==> field @"age")
