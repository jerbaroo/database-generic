{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE UndecidableInstances  #-}

module Database.Generic.Test.Statement where

import Database.Generic (PK, PrimaryKey, createTable)
import Database.Generic.Database (PostgreSQL)
import Database.Generic.Entity.SqlTypes (SqlTypeId(..))
import Database.Generic.Prelude
import Database.Generic.Statement.CreateTable (CreateTable(..), CreateTableColumn(..))
import GHC.Generics (Generic)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.SmallCheck qualified as SC
import Database.Generic.Serialize (Serialize(serialize))

data Person = Person { age :: !Int64, name :: !String }
  deriving (Eq, Generic, Show)
  deriving PrimaryKey via PK "name" Person

statementTests :: TestTree
statementTests = testGroup "Statement Tests" [createTableTests]

createTablePerson :: Bool -> CreateTable a
createTablePerson ifNotExists =
  CreateTable
    { columns =
        [ CreateTableColumn
            { name = "age"
            , primary = False
            , type' = SqlBigIntT
            }
        , CreateTableColumn
            { name = "name"
            , primary = True
            , type' = SqlLongVarCharT
            }
        ]
    , ifNotExists
    , name = "person"
    }

createTablePersonPG :: Bool -> String
createTablePersonPG ifNotExists = unwords $ catMaybes
  [ Just "CREATE TABLE"
  , if ifNotExists then Just "IF NOT EXISTS" else Nothing
  , Just "person (age BIGINT, name VARCHAR PRIMARY KEY);"
  ]

createTableTests :: TestTree
createTableTests = testGroup "Create Table tests"
  [ SC.testProperty "createTable @Person" \b ->
      createTable @Person b == createTablePerson b
  , SC.testProperty "serialize CreateTable Person" \b ->
      serialize @_ @PostgreSQL (createTablePerson b) == createTablePersonPG b
  ]
