module Main where

import Sygus.LexSygus
import Sygus.ParseSygus
import Sygus.Print

import Control.DeepSeq
import Data.Text (unpack)
import Test.Tasty
import Test.Tasty.HUnit

import Debug.Trace

main :: IO ()
main = do
  pt <- parseTests
  pr <- parseAndPrintsTests
  defaultMain $ testGroup "all" [ pt, pr ]

parseTests :: IO TestTree
parseTests = return . testGroup "Tests" =<< mapM checkParses files

parseAndPrintsTests :: IO TestTree
parseAndPrintsTests =
    return . testGroup "Tests" =<< mapM checkParsesAndPrints files

files :: [FilePath]
files =
    [ "tests/sygus/example1.sl"
    , "tests/sygus/example2.sl"
    , "tests/sygus/example3.sl"
    , "tests/sygus/example4.sl"
    , "tests/sygus/example5.sl"
    , "tests/sygus/example6.sl" ]

checkParses :: FilePath -> IO TestTree
checkParses fp = do
    s <- readFile fp
    let p = parse . lexSygus $ s

    return $ testCase fp
              $ assertBool fp
                (show p `deepseq` True)

checkParsesAndPrints :: FilePath -> IO TestTree
checkParsesAndPrints fp = do
    s <- readFile fp
    let p1 = parse . lexSygus $ s
        p2 = parse . lexSygus . unpack . printSygus $ p1

    return $ testCase fp
              $ assertBool fp
                (trace (show p1 ++ "\n" ++ show p2) p1 == p2)