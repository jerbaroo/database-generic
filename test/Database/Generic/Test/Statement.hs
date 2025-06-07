{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Database.Generic.Test.Statement where

import Database.Generic
import Database.Generic.Database (PostgreSQL)
import Database.Generic.Entity.SqlTypes (SqlTypeId(..))
import Database.Generic.Prelude
import Database.Generic.Serialize (Serialize(serialize))
import Database.Generic.Statement.CreateTable (CreateTable(..), CreateTableColumn(..))
import Database.Generic.Statement.Delete (Delete(..))
import Database.Generic.Statement.Type.OneOrMany (OneOrMany(..))
import GHC.Generics (Generic)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual)
import Test.Tasty.SmallCheck qualified as SC

statementTests :: TestTree
statementTests = testGroup "Statement tests"
  [ createTableTests
  , deleteTests
  ]

-- * Helpers.

data Person = Person { age :: !Int64, name :: !String }
  deriving (Eq, Generic, PrimaryKey "name", Show)

-- * Create table tests.

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
createTableTests = testGroup "Create table statement tests"
  [ SC.testProperty "createTable @Person" \b ->
      createTable @Person b == createTablePerson b
  , SC.testProperty "serialize CreateTable Person" \b ->
      serialize @_ @PostgreSQL (createTablePerson b) == createTablePersonPG b
  ]

-- * Delete tests.

deleteAllPerson :: Delete Many Nothing Person
deleteAllPerson = Delete
  { fields = Nothing
  , from   = "person"
  , where' = Nothing
  }

deleteTests :: TestTree
deleteTests = testGroup "Delete statement tests"
  [ testCase "deleteAll @Person" $ assertEqual ""
      (deleteAll @Person) deleteAllPerson
  ]

-- * Returning.

-- | This is a test that returning modifies the type correctly.
deleteAllPersonReturning :: Delete Many (Just Person) Person
deleteAllPersonReturning = returning $ deleteAll @Person

-- | This is a test that returning modifies the type correctly.
deleteByIdReturning :: Delete One (Just Person) Person
deleteByIdReturning = returning $ deleteById "john"

-- | This is a test that returningFields modifies the type correctly.
deleteByIdReturningTwoFields :: Delete One (Just (Int64, String)) Person
deleteByIdReturningTwoFields =
  returningFields (deleteById "john") $ field2 @"age" @"name"
