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

{-# LANGUAGE ExistentialQuantification #-}
module Document where

import Control.Monad.Reader
import Data.List (intercalate, sort)
import Graphics.PDF
import Text.JSON

import Constants
import LineItem
import Organization

data Document name =
    Document
    { dRevised :: String
    , dOrganizations :: [Organization name] }
      
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

fontTitle, fontSubtitle :: PDFFont
fontTitle    = PDFFont Times_Bold 18
fontSubtitle = PDFFont Helvetica 10

pageMargin, titleInset, titleRise :: PDFFloat
pageMargin    = fromIntegral units_per_inch / 4.0
titleInset    = fromIntegral units_per_inch * 1.75
titleRise     = fromIntegral units_per_inch * 10.5

titleString :: PDFString
titleString   = toPDFString "PHONE DIRECTORY"

dateInset, dateRise :: PDFFloat
dateInset     = pageMargin + col_padding
dateRise      = fromIntegral units_per_inch * 10.0

modeInset, modeRise :: PDFFloat
modeInset     = titleInset
modeRise      = dateRise

mkDocument :: Document a
mkDocument = Document
             { dRevised = "1/1/2009"
             , dOrganizations = []
             }

sortDoc :: (Ord a) => Document a -> Document a
sortDoc d =
    d { dOrganizations = map sortOrg $ 
                         sort (dOrganizations d) }
        
-- | Draw a Document.
renderDoc :: forall a. (Show a, Ord a) => Document a -> String -> PDF()
renderDoc d lbl= 
    let revised = toPDFString $ "Revised: " ++ dRevised d
        lineItems = intercalate [Divider] $ map showLineItems $ dOrganizations d
        columns = flowCols lineItems 4
        colCoords = zipWith (:+) colXs $ repeat grid_rise
        colXs = map (+ pageMargin) $ iterate (+ col_width) 0
    in do
      p <- addPage Nothing
      drawWithPage p $ do
         drawText $ text fontTitle titleInset titleRise titleString
         drawText $ text fontSubtitle dateInset dateRise revised
         drawText $ text fontSubtitle modeInset modeRise $ toPDFString lbl
         zipWithM_ (runReaderT . drawColumn) columns colCoords
