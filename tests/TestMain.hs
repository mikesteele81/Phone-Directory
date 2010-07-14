module Main where

import qualified TestContactInfo as CI
import qualified TestName as N
import qualified TestOrganization as O

main :: IO ()
main = do
    N.main
    CI.main
    O.main