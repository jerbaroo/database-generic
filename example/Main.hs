{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE OverloadedRecordDot #-}

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
    case debug $ outputType @s of
      OutputTypeAffected -> OutputAffected . debug <$> HDBC.run conn x []
      OutputTypeNada     -> debug OutputNada <$  HDBC.runRaw conn x
      OutputTypeRows     -> OutputRows . debug <$> HDBC.quickQuery' conn x []

-- | Enable 'AppM' to connect to PostgreSQL.
instance MonadDbNewConn AppM PSQL.Connection where
  newDbConn = liftIO . PSQL.connectPostgreSQL =<< ask

main :: IO ()
main = do
  let e    = env "127.0.0.1" 5432 "postgres" "demo" "demo"
  let john = Person "John" 21
  let br = putStrLn "\n"
  br -- Create table if not exists, twice.
  runAppM e $ tx_ $ execute $ createTable @Person True
  br -- Delete All.
  print =<< runAppM e (tx $ execute $ returning $ deleteAll @Person)
  br -- Delete by ID.
  print =<< runAppM e (tx $ execute $ deleteById @Person "John")
  br -- Insert one.
  print =<< runAppM e (tx $ execute $ returning $ insertOne $ john{age=55 })
  br -- Insert two.
  print =<< runAppM e (tx $ execute $ returning (insertMany [john{name="Foo"}, john {name = "Mary"}]) ==> field @"age" @Person)
  br -- Select by ID.
  print =<< runAppM e (tx_ $ execute $ selectById @Person john.name)
  br -- Select All.
  print =<< runAppM e (tx_ $ execute $ selectAll @Person)
  br -- Select specific fields by ID.
  print =<< runAppM e ( tx_ $ execute $
    selectById @Person john.name ==> field @"age" @Person )
