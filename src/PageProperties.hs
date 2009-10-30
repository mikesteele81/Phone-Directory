{- This file is part of PhoneDirectory.
   Copyright (C) 2009 Michael Steele

   PhoneDirectory is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   PhoneDirectory is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with PhoneDirectory.  If not, see <http://www.gnu.org/licenses/>.
-}

module PageProperties
    ( PageProperties (..)
    , mkPageProperties
    ) where

import Control.Applicative
import Control.Monad.Error
import Text.JSON
import Text.JSON.Pretty

import UnitConversion

data PageProperties = PageProperties
    { pageWidth    :: Inches
    , pageHeight   :: Inches
    , leftMargin   :: Inches
    , rightMargin  :: Inches
    , topMargin    :: Inches
    , bottomMargin :: Inches }
    deriving (Eq, Show)

instance JSON PageProperties where
    readJSON (JSObject o) = do
        pw <- (valFromObj "pageWidth"    o <|> (return . Inches) 8.5)
        ph <- (valFromObj "pageHeight"   o <|> (return . Inches) 11.0)
        lm <- (valFromObj "leftMargin"   o <|> return quarterInch)
        rm <- (valFromObj "rightMargin"  o <|> return quarterInch)
        tm <- (valFromObj "topMargin"    o <|> return quarterInch)
        bm <- (valFromObj "bottomMargin" o <|> return quarterInch)
        return $ PageProperties pw ph lm rm tm bm
    readJSON v = Error $ "Expected JSObject, but " ++ (show . pp_value) v
        ++ " found while parsing a contact information."
    showJSON pp = makeObj
        [ ("pageWidth"   , showJSON . pageWidth    $ pp)
        , ("pageHeight"  , showJSON . pageHeight   $ pp)
        , ("leftMargin"  , showJSON . leftMargin   $ pp)
        , ("rightMargin" , showJSON . rightMargin  $ pp)
        , ("topMargin"   , showJSON . topMargin    $ pp)
        , ("bottomMargin", showJSON . bottomMargin $ pp) ]

mkPageProperties :: PageProperties
mkPageProperties = PageProperties
    { pageWidth    = Inches 8.5
    , pageHeight   = Inches 11.0
    , leftMargin   = quarterInch
    , rightMargin  = quarterInch
    , topMargin    = quarterInch
    , bottomMargin = quarterInch }

quarterInch :: Inches
quarterInch = Inches 0.25