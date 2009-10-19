{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module UnitConversion where

import Graphics.PDF (PDFFloat)
import Text.JSON
import Text.JSON.Pretty

newtype Inches = Inches { unInches :: Double }
    deriving (Eq, Fractional, Num, Show)
newtype PDFUnits = PDFUnits { unPDFUnits :: PDFFloat }
    deriving (Eq, Fractional, Num, Show)

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