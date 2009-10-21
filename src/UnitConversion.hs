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


{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module UnitConversion where

import Graphics.PDF (PDFFloat)
import Text.JSON
import Text.JSON.Pretty

newtype Inches = Inches { unInches :: Double }
    deriving (Eq, Fractional, Num, Ord, Show)
newtype PDFUnits = PDFUnits { unPDFUnits :: PDFFloat }
    deriving (Eq, Fractional, Num, Ord, Show)

sixteenthInch :: Inches
sixteenthInch = Inches $ 1 / 16.0

instance JSON Inches where
    readJSON (JSRational _ v) = return . Inches . fromRational $ v
    readJSON v = Error $ "Expected JSString, but " ++ (show . pp_value) v
        ++ " found while parsing a contact information."
    showJSON = JSRational False . toRational . unInches

instance JSON PDFUnits where
    readJSON (JSRational _ v) = return . PDFUnits . fromRational $ v
    readJSON v = Error $ "Expected JSString, but " ++ (show . pp_value) v
        ++ " found while parsing a contact information."
    showJSON = JSRational False . toRational . unPDFUnits

class CInches a where
    asInches :: a -> Inches
class CPDFUnits a where
    asPDFUnits :: a -> PDFUnits

instance CInches Inches where asInches = id
instance CPDFUnits PDFUnits where asPDFUnits = id

instance CPDFUnits Inches where
    asPDFUnits (Inches v) = PDFUnits $ 72.0 * v

instance CInches PDFUnits where
    asInches (PDFUnits v) = Inches $ v / 72.0