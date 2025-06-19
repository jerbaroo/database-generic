{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Generic.Test.Statement where

import Database.Generic
import Database.Generic.Database (PostgreSQL)
import Database.Generic.Entity.DbTypes (DbT(..), Unit(Unit), DbTypeN (..))
import Database.Generic.Prelude
import Database.Generic.Serialize (Serialize(..))
import Database.Generic.Serialize qualified as Serialize
import Database.Generic.Statement.CreateTable (CreateTable, CreateTable'(..), CreateTableColumn(..))
import Database.Generic.Statement.Delete (Delete, Delete'(..))
import Database.Generic.Statement.Fields (Fields(..), OrderedFields (OrderedFields))
import Database.Generic.Statement.Limit (Limit, Offset, Limitable (limitOffsetMay))
import Database.Generic.Statement.OrderBy qualified as O
import Database.Generic.Statement.Select (Select, Select'(..))
import Database.Generic.Statement.Type.OneOrMany (OneOrMany(..))
import Database.Generic.Statement.Where (Where(Equals))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual)
import Test.Tasty.SmallCheck qualified as SC
import Witch qualified as W

statementTests :: TestTree
statementTests = testGroup "Statement tests"
  [ createTableTests
  , deleteTests
  , selectTests
  ]

-- * Helpers.

data Person = Person { age :: !Int64, name :: !String, ownsDog :: !(Maybe Bool)}
  deriving (Eq, Generic, PrimaryKey "name", Show)

-- * Create table tests.

createTablePerson :: Bool -> CreateTable a
createTablePerson ifNotExists =
  W.from $ CreateTable'
    { columns =
        [ CreateTableColumn
            { name = "age"
            , primary = False
            , type' = DbTypeN False $ DbInt64 Unit
            }
        , CreateTableColumn
            { name = "name"
            , primary = True
            , type' = DbTypeN False $ DbString Unit
            }
        , CreateTableColumn
            { name = "ownsdog"
            , primary = False
            , type' = DbTypeN True $ DbBool Unit
            }
        ]
    , ifNotExists
    , name = "person"
    }

createTablePersonPG :: Bool -> String
createTablePersonPG ifNotExists = unwords $ catMaybes
  [ Just "CREATE TABLE"
  , if ifNotExists then Just "IF NOT EXISTS" else Nothing
  , Just "person (age BIGINT NOT NULL, name VARCHAR NOT NULL PRIMARY KEY, ownsdog BOOLEAN);"
  ]

createTableTests :: TestTree
createTableTests = testGroup "Create table statement tests"
  [ SC.testProperty "createTable @Person" \b ->
      createTable @Person b == createTablePerson b
  , SC.testProperty "serialize CreateTable Person" \b ->
      serialize @_ @PostgreSQL (into @CreateTable' $ createTable @Person b)
        == createTablePersonPG b
  ]

-- * Delete tests.

deleteAllPerson :: Delete Many Nothing Person
deleteAllPerson = W.from Delete'
  { fields = Nothing
  , from   = "person"
  , where' = Nothing
  }

-- | This is a test that 'returning' modifies the type correctly.
deleteAllPersonReturning :: Delete Many (Just Person) Person
deleteAllPersonReturning = returning deleteAll

-- | This is a test that 'returning' modifies the type correctly.
deleteByIdReturning :: Delete One (Just Person) Person
deleteByIdReturning = returning $ deleteById "john"

-- | This is a test that 'returningFields' modifies the type correctly.
deleteByIdReturningTwoFields :: Delete One (Just (Int64, String)) Person
deleteByIdReturningTwoFields =
  returningFields (deleteById "john") $ field @"age" /\ field @"name"

deleteTests :: TestTree
deleteTests = testGroup "Delete statement tests"
  [ testCase "deleteAll @Person" $ assertEqual "" deleteAll deleteAllPerson
  , testCase "serialize deleteAll @Person" $ assertEqual ""
      "DELETE FROM person;" $
      serialize @_ @PostgreSQL $ into @Delete' $ deleteAll @Person
  , testCase "serialize deleteById @Person" $ assertEqual ""
      "DELETE FROM person WHERE name='Mary';" $
      serialize @_ @PostgreSQL $ into @Delete' $ deleteById @Person "Mary"
  ]

-- * Select tests.

selectAllPerson :: Select Many Person Person False
selectAllPerson = W.from Select'
  { fields  = All
  , from    = "person"
  , limit   = Nothing
  , offset  = Nothing
  , orderBy = OrderedFields []
  , where'  = Nothing
  }

-- | This is a test that 'O.orderBy' modifies the type correctly.
selectAllPersonOrderByName :: Select Many Person Person True
selectAllPersonOrderByName =
  O.orderBy (order @"name" @Asc) $ selectAll @Person

selectByIdPerson :: Select One Person Person False
selectByIdPerson = W.from Select'
  { fields  = All
  , from    = "person"
  , limit   = Nothing
  , offset  = Nothing
  , orderBy = OrderedFields []
  , where'  = Just $ Equals "name" $ Just $ DbString "John"
  }

selectAllPersonPG :: Limit -> Maybe Offset -> String
selectAllPersonPG limit offset = Serialize.statement $ unwords $ catMaybes
  [ Just $ "SELECT * FROM person ORDER BY age ASC LIMIT " <> show limit
  , offset <&> \l -> "OFFSET " <> show l
  ]

selectTests :: TestTree
selectTests = testGroup "Select statement tests"
  [ testCase "selectAll @Person" $ assertEqual "" selectAllPerson selectAll
  , testCase "selectById @Person" $ assertEqual "" selectByIdPerson $ selectById "John"
  , SC.testProperty "serialize limit offset" \(l, o) ->
      selectAllPersonPG l o == serialize @_ @PostgreSQL
        (into @Select'
           $ limitOffsetMay l o
           $ O.orderBy (order @"age" @Asc)
           $ selectAll @Person
        )
  ]
