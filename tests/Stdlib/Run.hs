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

import Scheme.Eval(evalText, evalFile, initialState)
import Scheme.Exceptions(catchHaskellExceptions, defaultExceptionHandler)
import Scheme.LispVal(showVal)

mkTests :: [(FilePath, T.Text)] -> [TestTree]
mkTests = map (\(fn, expected) -> testCase (takeFileName fn) $ do
                  actual <- run fn
                  assertEqual "" (T.strip expected) (T.strip actual))

run :: FilePath -> IO T.Text
run fn = do
    -- Read in the standard library.
    stdlib       <- TIO.readFile "library.scm"
    (val, state) <- evalFile initialState stdlib
    -- We only care about val here because it could be an error.  If it is, this
    -- will cause it to be handled correctly.  If it's not an error, this call will
    -- do nothing so it's safe call regardless.
    defaultExceptionHandler val

    s <- TIO.readFile fn
    (ret, _) <- catchHaskellExceptions $ evalText state s
    return $ showVal ret
