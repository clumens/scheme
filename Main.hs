{-# LANGUAGE OverloadedStrings #-}

import           Control.Conditional(whenM)
import qualified Data.Text.IO as TIO
import           System.Directory(doesFileExist)
import           System.Environment(getArgs)

import Scheme.Eval(evalFile, evalText, initialState)
import Scheme.Exceptions(catchHaskellExceptions, defaultExceptionHandler)
import Scheme.Repl(mainLoop)

main :: IO ()
main = do
    args <- getArgs

    -- Read in the standard library.
    contents     <- TIO.readFile "library.scm"
    (val, state) <- evalFile initialState contents
    -- We only care about val here because it could be an error.  If it is, this
    -- will cause it to be handled correctly.  If it's not an error, this call will
    -- do nothing so it's safe call regardless.
    defaultExceptionHandler val

    -- If any files were provided on the command line, run them all in their own
    -- environment and print out any errors.  If the command line was empty, run the
    -- REPL instead.  It will do its own error handling.
    if not (null args)
    then mapM_ (\arg -> whenM (doesFileExist arg) $ do
                            s        <- TIO.readFile arg
                            (ret, _) <- catchHaskellExceptions $ evalText state s
                            defaultExceptionHandler ret)
               args
    else mainLoop state
