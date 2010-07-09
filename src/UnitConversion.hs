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

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module UnitConversion where

import Control.Applicative
import Graphics.PDF (PDFFloat)
import Data.Attempt
import Data.Convertible.Base
import Data.Object.Json

newtype Inches = Inches { unInches :: Double }
    deriving (Eq, Fractional, Num, Ord, Show)
newtype PDFUnits = PDFUnits { unPDFUnits :: PDFFloat }
    deriving (Eq, Fractional, Num, Ord, Show)

sixteenthInch :: Inches
sixteenthInch = Inches $ 1 / 16.0

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

instance ConvertSuccess Inches JsonScalar where
  convertSuccess (Inches i) = JsonNumber (convertSuccess i)

instance ConvertAttempt JsonScalar Inches where
  convertAttempt (JsonNumber i)  = Inches <$> convertAttempt i
  convertAttempt _ = failure NothingException

instance ConvertSuccess PDFUnits JsonScalar where
  convertSuccess (PDFUnits i) = JsonNumber (convertSuccess i)

instance ConvertAttempt JsonScalar PDFUnits where
  convertAttempt (JsonNumber i)  = PDFUnits <$> convertAttempt i
  convertAttempt _ = failure NothingException
