module Main where

import qualified TestContactInfo as CI
import qualified TestDocument as D
import qualified TestLineItem as LI
import qualified TestName as N
import qualified TestOrganization as O
import qualified TestPageProperties as PP
import qualified TestUnitConversion as UC

main :: IO ()
main = do
    CI.main
    D.main
    LI.main
    N.main
    O.main
    PP.main
    UC.main
