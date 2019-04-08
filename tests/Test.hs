module Main where

import Sygus.LexSygus
import Sygus.ParseSygus

import Control.DeepSeq
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = do
  pt <- parseTests
  defaultMain $ testGroup "all" [ pt ]

parseTests :: IO TestTree
parseTests = return . testGroup "Tests" =<< sequence [ checkParses "tests/sygus/constraint1.sl" ]

checkParses :: FilePath -> IO TestTree
checkParses fp = do
    s <- readFile fp
    let p = parse . lexSygus $ s

    return $ testCase fp
              $ assertBool fp
                (show p `deepseq` True)