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

module TestUnitConversion where

import Control.Applicative
import Test.QuickCheck

import UnitConversion

prop_reflective_inch_pdf_conversion :: Inches -> Bool
prop_reflective_inch_pdf_conversion i = abs(i' - i) < 0.0001
  where
    i' = asInches . asPDFUnits . asInches . asPDFUnits $ i

main :: IO ()
main = do
    putStrLn "Inches & PDFUnits: Reflective conversion."
    quickCheck prop_reflective_inch_pdf_conversion

instance Arbitrary Inches where
    arbitrary = Inches <$> arbitrary
    shrink (Inches x) = map Inches (shrink x)

instance Arbitrary PDFUnits where
    arbitrary = PDFUnits <$> arbitrary
    shrink (PDFUnits x) = map PDFUnits (shrink x)
