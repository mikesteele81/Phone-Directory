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

import Control.Monad.Reader
import Data.List (sort)
import Graphics.PDF
import Text.JSON

import LineItem
import Organization
import PDF

-- |A Document contains a revision date and organizations to display.
data Document a
  = Document
    { -- |This gets set whenever the document is ready to be
      -- printed. Because of this, it's not really necessary at this
      -- point.
      dRevised :: String
      -- |Organizations to print.
    , dOrganizations :: [Organization a]
    } deriving (Eq)
      
instance (JSON a) => JSON (Document a) where
    readJSON (JSObject d) =
        do revised <- valFromObj "revised" d
           organizations <- valFromObj "organizations" d
           return $ Document revised organizations
    readJSON _ = Error "Could not parse Document JSON object."
    showJSON d =
        showJSON $ toJSObject
        [ ("revised", showJSON $ dRevised d)
        , ("organizations", showJSONs $ dOrganizations d) ]

-- Perform an operation on the name.  Is this an abuse of Functors?
instance Functor Document where
    f `fmap` d = d { dOrganizations = map (fmap f) $ dOrganizations d }

-- |Font used only for the title.
fontTitle :: PDFFont
fontTitle    = PDFFont Times_Bold 18

-- |Font used for all header information besides the title.
fontSubtitle :: PDFFont
fontSubtitle = PDFFont Helvetica 10

-- |Margin used for entire page.
pageMargin :: PDFFloat
pageMargin = fromIntegral units_per_inch / 4.0

-- |Distance from left edge to draw the title.
titleInset :: PDFFloat
titleInset = fromIntegral units_per_inch * 1.75

-- |Distance from bottom edge to draw the title.
titleRise :: PDFFloat
titleRise = fromIntegral units_per_inch * 10.5

titleString :: PDFString
titleString = toPDFString "PHONE DIRECTORY"

-- |Distance from left edge to draw the date string.
dateInset :: PDFFloat
dateInset = pageMargin + fromIntegral units_per_inch / 16.0

-- |Distance from bottom edge to draw the date string.
dateRise :: PDFFloat
dateRise = fromIntegral units_per_inch * 10.0

-- |Distance from left edge to draw the sort specifier.
modeInset :: PDFFloat
modeInset = titleInset

-- |Distance from bottom edge to draw the sort specifier.
modeRise :: PDFFloat
modeRise = dateRise

grid_rise :: PDFFloat
grid_rise = fromIntegral units_per_inch * 9.75

-- |Convenient way to make a Document.
mkDocument :: Document a
mkDocument = Document { dRevised = "", dOrganizations = [] }

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
        op coord col = runReaderT (draw col) coord
    in do
      p <- addPage Nothing
      drawWithPage p $ do
         drawText $ text fontTitle titleInset titleRise titleString
         drawText $ text fontSubtitle dateInset dateRise revised
         drawText $ text fontSubtitle modeInset modeRise $ toPDFString lbl
         foldM_ op (pageMargin :+ grid_rise) columns
         return ()
