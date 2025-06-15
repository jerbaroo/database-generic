{-# LANGUAGE OverloadedStrings #-}

module Database.Generic.Server.API where

import Database.Generic.Statement.NoType qualified as NT
import Database.Generic.Statement.Output (Output, OutputType)
import Database.Generic.Prelude
import Servant.API

type API dbv =
  "executeStatement"
  :> ReqBody '[JSON] (NT.Statement, OutputType)
  :> Post '[JSON] (Either String (Output dbv))
