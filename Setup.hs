#!/usr/bin/env runhaskell
import qualified Distribution.PackageDescription as P
import Distribution.Simple
import qualified Distribution.Simple.LocalBuildInfo as L
import System.FilePath
import System.Process

main :: IO ()
main = defaultMainWithHooks
       $ simpleUserHooks { runTests = tests }

tests :: Args -> Bool -> P.PackageDescription -> L.LocalBuildInfo -> IO ()
tests _ _ _ lbi = system testprog >> return ()
  where
    testprog = (L.buildDir lbi) </> "test" </> "test"
