{-# LANGUAGE ExistentialQuantification #-}
module Document where

import Control.Monad (zipWithM_)
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
        lineItems = intercalate [Spacer] $ map showLineItems $ dOrganizations d
        columns = flowCols lineItems 4
        colCoords = zipWith (:+) colXs $ repeat grid_rise
        colXs = map (+ page_margin) $ iterate (+ col_width) 0
    in do
      p <- addPage Nothing
      drawWithPage p $ do
         drawText $ text font_title title_inset title_rise title_string
         drawText $ text font_normal date_inset date_rise revised
         drawText $ text font_normal mode_inset mode_rise $ toPDFString lbl
         zipWithM_ renderCol columns colCoords

-- |Draw a column
renderCol :: Column -> Point -> Draw ()
renderCol c p@(x :+ y) =
  let
    br = ( col_width
           :+ (-1 * (fromIntegral . (+3) . length) c * line_item_leading))
  in do
    drawText $ do
      textStart (x + col_padding) (y - line_item_leading)
      drawColumn c
    stroke (Rectangle p (p + br))
