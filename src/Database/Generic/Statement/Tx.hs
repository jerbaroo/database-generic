module Database.Generic.Statement.Tx where

import Database.Generic.Serialize (Serialize(..))

data BeginTx = BeginTx

instance Serialize BeginTx db where
  serialize _ = "BEGIN TRANSACTION";

data CommitTx = CommitTx

instance Serialize CommitTx db where
  serialize _ = "COMMIT TRANSACTION";
