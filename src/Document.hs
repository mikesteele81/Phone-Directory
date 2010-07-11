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
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Document
    ( Document (..)
    , mkDocument
    , renderDoc
    , sortDoc
    , toCSVRecords
    ) where

import Control.Applicative
import Control.Monad (foldM_)
import qualified Data.ByteString.Char8 as B
import Data.Convertible.Base
import Data.List (sort)
import Data.Object
import qualified Data.Object.Json as J
import Graphics.PDF
import Text.CSV (Record)

import ContactInfo
import LineItem
import qualified Organization as O
import PageProperties
import UnitConversion

-- |A Document contains a revision date and organizations to display.
data Document a
  = Document
    { -- |This gets set whenever the document is ready to be
      -- printed. Because of this, it's not really necessary at this
      -- point.
      dRevised :: String
      -- |Organizations to print.
    , dOrganizations :: [O.Organization a]
    , pageProperties :: PageProperties
    } deriving (Eq, Show)
      
instance ConvertAttempt J.JsonObject a
    => ConvertAttempt J.JsonObject (Document a) where
 convertAttempt j =
     do m  <- fromMapping j
        r  <- J.fromJsonScalar <$> lookupScalar (B.pack "revised") m
        ox <- lookupSequence (B.pack "organizations") m >>= mapM convertAttempt
        p  <- lookupObject (B.pack "pageProperties") m >>= convertAttempt
        
        return $ Document r ox p

instance (ConvertSuccess a J.JsonObject)
    => ConvertSuccess (Document a) J.JsonObject where
  convertSuccess (Document r ox pp) =
      Mapping [ (B.pack "revised", Scalar $ J.toJsonScalar r)
              , (B.pack "organizations", Sequence $ map convertSuccess ox)
              , (B.pack "pageProperties", convertSuccess pp)]

-- Perform an operation on the name.  Is this an abuse of Functors?
instance Functor Document where
    f `fmap` d = d { dOrganizations = map (fmap f) $ dOrganizations d }

toCSVRecords :: Show a => Document (ContactInfo a) -> [Record]
toCSVRecords (Document _ ox _) = concatMap O.toCSVRecords ox

-- |Font used only for the title.
fontTitle :: PDFFont
fontTitle    = PDFFont Times_Bold 18

-- |Font used for all header information besides the title.
fontSubtitle :: PDFFont
fontSubtitle = PDFFont Helvetica 10

titleString :: PDFString
titleString = toPDFString "PHONE DIRECTORY"

titleWidth :: PDFUnits
titleWidth = PDFUnits $ textWidth fontTitle titleString

-- |Distance from left edge to draw the date string.
dateInset :: PageProperties -> PDFUnits
dateInset p = asPDFUnits $ leftMargin p + Inches (1 / 16)

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
    d { dOrganizations = map O.sortOrg $ sort (dOrganizations d) }
        
-- | Draw a Document on its own page.
renderDoc :: (ShowLineItems a, Ord a)
  => Document a -- ^Document to append a page for.
  -> String     -- ^Subtitle
  -> PDF()
renderDoc d lbl= 
    let revised = toPDFString $ "Revised: " ++ dRevised d
        columns = flowCols (showLineItems $ dOrganizations d) 4
        dateRise = titleRise - (PDFUnits $ getHeight fontSubtitle)
            - asPDFUnits sixteenthInch
        gridRise = dateRise - asPDFUnits sixteenthInch
        lbl' = toPDFString lbl
        lblInset = (width - (PDFUnits $ textWidth fontSubtitle lbl')) / 2.0
        prop = pageProperties d
        colWidth = asPDFUnits $ (pageWidth prop - leftMargin prop - rightMargin prop) / 4.0
        titleInset = (width - titleWidth) / 2.0
        titleRise = height - (asPDFUnits . topMargin $ prop)
            - (PDFUnits $ getHeight fontTitle)
        width = asPDFUnits . pageWidth $ prop
        height = asPDFUnits . pageHeight $ prop
    in do
      p <- addPage Nothing
      drawWithPage p $ do
         drawText $ text fontTitle (unPDFUnits titleInset)
             (unPDFUnits titleRise) titleString
         drawText $ text fontSubtitle (unPDFUnits . dateInset $ prop)
             (unPDFUnits dateRise)  revised
         drawText $ text fontSubtitle (unPDFUnits lblInset)
             (unPDFUnits dateRise) lbl'
         foldM_ (drawColumn colWidth)
             ((unPDFUnits . asPDFUnits . leftMargin $ prop)
             :+ (unPDFUnits gridRise)) columns
