{-# LANGUAGE ExistentialQuantification #-}
module Document where

import Control.Monad (zipWithM_)
import Data.List (sort)
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
        
mkDocument :: Document a
mkDocument = Document
             { dRevised = "1/1/2009"
             , dOrganizations = []
             }

sortDoc :: (Ord a) => Document a -> Document a
sortDoc d =
    d { dOrganizations = map sortOrg $ 
                         sort (dOrganizations d) }
        
-- | Draw a Document.  It renders at least 2 pages in the PDF monad.
renderDoc :: forall a. (Show a, Ord a) => Document a -> PDF()
renderDoc d = 
    let page1 = sortDoc d
        revised = toPDFString $ "Revised: " ++ dRevised page1
        lineItems = map showLineItems $ dOrganizations page1
        columns = flowCols lineItems 4
        colXs = map (+ page_margin) $ take 4
                $ iterate (+ (line_item_width + line_item_leading)) 0
        drawCol colX c = drawText $ do
                           textStart colX grid_rise
                           drawColumn c
    in do
      p <- addPage Nothing
      drawWithPage p $ do
         drawText $ text font_title title_inset title_rise title_string
         drawText $ text font_normal date_inset date_rise revised
         drawText $ text font_normal mode_inset mode_rise mode_string
         zipWithM_ drawCol colXs columns
         beginPath (300 :+ 300)
         lineto (350 :+ 320)
         strokePath
         closePath
         drawText $ text font_normal 0.0 0.0 $ toPDFString "!"
