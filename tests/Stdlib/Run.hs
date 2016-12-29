module Stdlib.Run(mkTests,
                  run)
 where

import           Control.Applicative((<$>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           System.FilePath.Posix(takeFileName)
import           Test.HUnit(assertEqual)
import           Test.Tasty(TestTree)
import           Test.Tasty.HUnit(testCase)

import Scheme.Eval(basicEnv, evalText, execFile)
import Scheme.LispVal(showVal)

mkTests :: [(FilePath, T.Text)] -> [TestTree]
mkTests testData = map (\(fn, expected) -> testCase (takeFileName fn) $ do
                           actual <- run fn
                           assertEqual "" (T.strip expected) (T.strip actual))
                   testData

run :: FilePath -> IO T.Text
run fn = do
    stdlib <- TIO.readFile "library.scm"
    env <- execFile basicEnv stdlib

    s <- TIO.readFile fn
    showVal <$> evalText env s
