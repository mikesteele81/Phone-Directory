{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module UnitConversion where

import Graphics.PDF (PDFFloat)

newtype Inches = Inches { unInches :: Double }
    deriving (Eq, Fractional, Num, Show)
newtype PDFUnits = PDFUnits { unPDFUnits :: PDFFloat }
    deriving (Eq, Fractional, Num, Show)

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