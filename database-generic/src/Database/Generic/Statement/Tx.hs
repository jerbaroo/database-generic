module Database.Generic.Statement.Tx where

import Data.Aeson qualified as Aeson
import Database.Generic.Prelude
import Database.Generic.Serialize (Serialize(..))

data BeginTx = BeginTx deriving Generic

instance Aeson.FromJSON BeginTx

instance Serialize BeginTx db where
  serialize _ = "BEGIN TRANSACTION";

data CommitTx = CommitTx deriving Generic

instance Aeson.FromJSON CommitTx

instance Serialize CommitTx db where
  serialize _ = "COMMIT TRANSACTION";
