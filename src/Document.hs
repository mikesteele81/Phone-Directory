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

module Document
    ( Document (..)
    , mkDocument
    , renderDoc
    , sortDoc
    ) where

import Control.Applicative
import Control.Monad (foldM_)
import Data.List (sort)
import Graphics.PDF
import Text.JSON

import LineItem
import Organization
import PageProperties
import PDF
import UnitConversion

-- |A Document contains a revision date and organizations to display.
data Document a
  = Document
    { -- |This gets set whenever the document is ready to be
      -- printed. Because of this, it's not really necessary at this
      -- point.
      dRevised :: String
      -- |Organizations to print.
    , dOrganizations :: [Organization a]
    , pageProperties :: PageProperties
    } deriving (Eq, Show)
      
instance (JSON a) => JSON (Document a) where
    readJSON (JSObject d) =
        do revised <- valFromObj "revised" d
           organizations <- valFromObj "organizations" d
           prop <- valFromObj "pageProperties" d <|> return mkPageProperties
           return $ Document revised organizations prop
    readJSON _ = Error "Could not parse Document JSON object."
    showJSON d =
        showJSON $ toJSObject
        [ ("revised", showJSON $ dRevised d)
        , ("organizations", showJSONs $ dOrganizations d)
        , ("pageProperties", showJSON . pageProperties $ d)]

-- Perform an operation on the name.  Is this an abuse of Functors?
instance Functor Document where
    f `fmap` d = d { dOrganizations = map (fmap f) $ dOrganizations d }

-- |Font used only for the title.
fontTitle :: PDFFont
fontTitle    = PDFFont Times_Bold 18

-- |Font used for all header information besides the title.
fontSubtitle :: PDFFont
fontSubtitle = PDFFont Helvetica 10

pageWidth :: PDFUnits
pageWidth = asPDFUnits . Inches $ 8.5

pageHeight :: PDFUnits
pageHeight = asPDFUnits . Inches $ 11.0

-- |Distance from left edge to draw the title.
titleInset :: PDFUnits
titleInset = (pageWidth - titleWidth) / 2.0

-- |Distance from bottom edge to draw the title.
titleRise :: PageProperties -> PDFUnits
titleRise p = pageHeight - (asPDFUnits . topMargin $ p)
    - (PDFUnits $ getHeight fontTitle)

titleString :: PDFString
titleString = toPDFString "PHONE DIRECTORY"

titleWidth :: PDFUnits
titleWidth = PDFUnits $ textWidth fontTitle titleString

-- |Distance from left edge to draw the date string.
dateInset :: PageProperties -> PDFUnits
dateInset p = asPDFUnits $ leftMargin p + Inches (1 / 16)

-- |Distance from bottom edge to draw the date string.
dateRise :: PageProperties -> PDFUnits
dateRise p = titleRise p - (PDFUnits $ getHeight fontSubtitle)
    - asPDFUnits sixteenthInch

-- |Distance from bottom edge to draw the sort specifier.
modeRise :: PageProperties -> PDFUnits
modeRise p = dateRise p

gridRise :: PDFUnits
gridRise = asPDFUnits . Inches $ 10.25

-- |Convenient way to make a Document.
mkDocument :: Document a
mkDocument = Document
    { dRevised = ""
    , dOrganizations = []
    , pageProperties = mkPageProperties }

-- |Deep sort the document and all organizations that are a part of it.
sortDoc :: (Ord a)
  => Document a -- ^Document to deep sort
  -> Document a -- ^An identical Document that has possibly been
                -- rearranged.
sortDoc d =
    d { dOrganizations = map sortOrg $ sort (dOrganizations d) }
        
-- | Draw a Document on its own page.
renderDoc :: (ShowLineItems a, Ord a)
  => Document a -- ^Document to append a page for.
  -> String     -- ^Subtitle
  -> PDF()
renderDoc d lbl= 
    let revised = toPDFString $ "Revised: " ++ dRevised d
        columns = flowCols (showLineItems $ dOrganizations d) 4
        lbl' = toPDFString lbl
        lblInset = (pageWidth - (PDFUnits $ textWidth fontSubtitle lbl')) / 2.0
        prop = pageProperties d
    in do
      p <- addPage Nothing
      drawWithPage p $ do
         drawText $ text fontTitle (unPDFUnits titleInset) (unPDFUnits $ titleRise prop)
             titleString
         drawText $ text fontSubtitle (unPDFUnits . dateInset $ prop)
             (unPDFUnits . dateRise $ prop)  revised
         drawText $ text fontSubtitle (unPDFUnits lblInset)
             (unPDFUnits . modeRise $ prop) lbl'
         foldM_ draw
             ((unPDFUnits . asPDFUnits . leftMargin $ prop)
             :+ unPDFUnits gridRise) columns
