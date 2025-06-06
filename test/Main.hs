{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module Main (main) where

import Data.Int (Int64)
import Database.Generic
import Database.Generic.Statement.CreateTable (CreateTable(..), CreateTableColumn(..))
import Database.Generic.Entity.SqlTypes
import GHC.Generics (Generic)
import Test.Tasty
import Test.Tasty.SmallCheck qualified as SC

data Person = Person { age :: !Int64, name :: !String }
  deriving (Eq, Generic, Show)

instance Entity Person where type PrimaryKey Person = "name"

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [statementTests]

statementTests :: TestTree
statementTests = testGroup "Statement Tests" [createTableTests]

createTableTests :: TestTree
createTableTests = testGroup "Create Table tests"
  [ SC.testProperty "createTable @Person" \ifNotExists ->
      createTable @Person ifNotExists == CreateTable
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
  ]
