{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main (main) where

import Data.Int (Int64)
import Database.Generic
import Database.Generic.Statement.CreateTable (CreateTable(..), CreateTableColumn(..))
import Database.Generic.Entity.SqlTypes
import GHC.Generics (Generic)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck qualified as SC

data Person = Person { age :: !Int64, name :: !String }
  deriving (Entity "name", Eq, Generic, Show)

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
        { fields =
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
