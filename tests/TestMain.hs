module Main where

import qualified TestContactInfo as CI
import qualified TestName as N
import qualified TestOrganization as O
import qualified TestPageProperties as PP
import qualified TestUnitConversion as UC

main :: IO ()
main = do
    N.main
    CI.main
    O.main
    PP.main
    UC.main
