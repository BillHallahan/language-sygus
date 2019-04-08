module Main where

import Test.Tasty

main :: IO ()
main = do
  defaultMain $ testGroup "all" [ tests ]

tests :: TestTree
tests =  testGroup "Tests" [ ]
