-- This tutorial uses GHC2024.

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies   #-}

module Main where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader(..), ReaderT(..))
import Data.ByteString.Char8 qualified as BS
import Data.Functor.Identity (Identity(..))
import Data.Int (Int64)
import Database.Generic
import Database.Generic.Database (PostgreSQL)
import Database.Generic.HDBC ()
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
data Person = Person { age :: !Int64, name :: !String, ownsDog :: !(Maybe Bool) }
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
instance MonadDb AppM PostgreSQL where
  type C AppM PostgreSQL = PSQL.Connection

  -- TODO move to HDBC module
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
  let c        = connStr "127.0.0.1" 5432 "postgres" "demo" "demo"
  let john     = Person 70 "John" $ Just False
  let info m s = do
        putStrLn $ "\n" <> m
        print =<< runAppM c (tx $ execute s)

  info "Create table if not exists" $ createTable @Person True
  info "Delete all" $ deleteAll @Person -- Clear table before tutorial.

  info "Insert one" $ insertOne john

  info "Insert many" $
    insertMany [Person 25 "Alice" $ Just True, Person 25 "Bob" Nothing]

  info "Insert many, returning" $
    returning $ insertMany [Person 26 "Charlie" Nothing, Person 26 "Dee" $ Just False]

  info "Insert many, returning age" $
    insertMany [Person 27 "Enid" Nothing, Person 27 "Flavio" Nothing] ==> field @"age"

  info "Select all" $ selectAll @Person

  info "Select by PK" $ selectById @Person "John"

  info "Select all, returning two fields" $
    selectAll @Person ==> (field @"age" /\ field @"name")

  info "Select all, order by age then name" $
    orderBy (order @"age" @Desc /\ order @"name" @Asc) $ selectAll @Person

  info "Select all, limit 1" $
    limit 1 $ orderBy (order @"name" @Asc) $ selectAll @Person

  info "Select all, order by name, limit 1, offset 2"
    $ limitOffset 1 2 $ orderBy (order @"name" @Asc) $ selectAll @Person

  info "Delete by PK" $ deleteById @Person "John"

  info "Delete all, returning" $ returning $  deleteAll @Person

  putStrLn "\nStarting a server which will proxy any statements"
  Server.run (runAppM c) 1234 Server.developmentCors
