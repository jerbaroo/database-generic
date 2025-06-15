{-# LANGUAGE OverloadedStrings #-}

module Database.Generic.Server where

import Control.Monad.IO.Class (MonadIO(..))
import Data.Aeson qualified as Aeson
import Database.Generic.Class (MonadDb(..), MonadDbWithConn(..))
import Database.Generic.Database (Database(..))
import Database.Generic.Statement.NoType qualified as NT
import Database.Generic.Statement.Output (Output, OutputType)
import Database.Generic.Prelude
import Network.Wai ( Middleware )
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Middleware.Cors qualified as Cors
import Servant (Handler, Server, serve)
import Servant.API

-- TODO API to separate package
type API dbv =
  "executeStatement"
  :> ReqBody '[JSON] (NT.Statement, OutputType)
  :> Post '[JSON] (Either String (Output dbv))

apiHandler
  :: forall m db
  .  ( Database db
     , MonadDb m db
     , MonadDbWithConn m (C m db)
     , Show (DbV db)
     , T m db ~ Identity
     , Typeable (DbV db)
     )
  => (forall a. m a -> IO a)
  -> (NT.Statement, OutputType)
  -> Handler (Either String (Output (DbV db)))
apiHandler nt (s, o) =
  fmap (mapLeft displayException  . extract) $ liftIO$ nt $ withDbConn \c ->
    executeStatement c $ Identity (s, o)

server
  :: forall m db
  . ( Database db
    , MonadDb m db
    , MonadDbWithConn m (C m db)
    , Show (DbV db)
    , T m db ~ Identity
    , Typeable (DbV db)
    )
  => (forall a. m a -> IO a)
  -> Server (API (DbV db))
server = apiHandler

developmentCors :: Middleware
developmentCors = Cors.cors $ const $ Just Cors.simpleCorsResourcePolicy
  { Cors.corsMethods        = ["OPTIONS", "POST"]
  , Cors.corsRequestHeaders = ["Content-Type"]
  }

run
  :: forall m db
  . ( Aeson.ToJSON (DbV db)
    , Database db
    , MonadDb m db
    , MonadDbWithConn m (C m db)
    , Show (DbV db)
    , T m db ~ Identity
    , Typeable (DbV db)
    )
  => (forall a. m a -> IO a)
  -> Int -> Middleware -> IO ()
run nt port middleware =
  Warp.run port $ middleware $ serve (Proxy @(API (DbV db))) $ server nt
