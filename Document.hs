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
        lineItems = map showLineItems $ dOrganizations d
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
         drawText $ text font_normal mode_inset mode_rise $ toPDFString lbl
         zipWithM_ drawCol colXs columns
         beginPath (300 :+ 300)
         lineto (350 :+ 320)
         strokePath
         closePath
         drawText $ text font_normal 0.0 0.0 $ toPDFString "!"
