{-# LANGUAGE OverloadedStrings #-}

module Database.Generic.Server where

import Control.Monad.IO.Class (MonadIO(..))
import Data.Aeson qualified as Aeson
import Database.Generic.Class (MonadDb(..), MonadDbWithConn(..))
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
  :: (MonadDb m Identity c dbv, MonadDbWithConn m c, Show dbv, Typeable dbv)
  => (forall a. m a -> IO a)
  -> (NT.Statement, OutputType)
  -> Handler (Either String (Output dbv))
apiHandler nt (s, o) =
  liftIO (nt $ withDbConn $ \c -> executeStatement c $ Identity (s, o))
    <&> mapLeft displayException  . extract

server
  :: (MonadDb m Identity c dbv, MonadDbWithConn m c, Show dbv, Typeable dbv)
  => (forall a. m a -> IO a)
  -> Server (API dbv)
server = apiHandler

developmentCors :: Middleware
developmentCors = Cors.cors $ const $ Just Cors.simpleCorsResourcePolicy
  { Cors.corsMethods        = ["OPTIONS", "POST"]
  , Cors.corsRequestHeaders = ["Content-Type"]
  }

run
  :: forall m c dbv
  . ( Aeson.ToJSON dbv
    , MonadDb m Identity c dbv
    , MonadDbWithConn m c
    , Show dbv
    , Typeable dbv
    )
  => (forall a. m a -> IO a)
  -> Int -> Middleware -> IO ()
run nt port middleware =
  Warp.run port $ middleware $ serve (Proxy @(API dbv)) $ server nt
