module Main (main) where

import Database.Generic.Prelude
import Database.Generic.Test.Statement (statementTests)
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [statementTests]
