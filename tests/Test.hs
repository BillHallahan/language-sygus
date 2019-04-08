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
parseTests = return . testGroup "Tests" =<< mapM checkParses
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