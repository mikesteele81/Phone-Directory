{-# LANGUAGE ExistentialQuantification #-}
module Document where

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
        showJSON $ toJSObject $
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
renderDoc :: forall a. (Show a) => Document a -> PDF()
renderDoc d = 
    let revised = toPDFString $ "Revised: " ++ dRevised d
        go x = do
          leading org_leading
          startNewLine
          drawLineItems line_item_width x
    in do
      p <- addPage Nothing
      drawWithPage p $ do
         drawText $ text font_title title_inset title_rise title_string
         drawText $ text font_normal date_inset date_rise revised
         drawText $ text font_normal mode_inset mode_rise mode_string
         drawText $ do
           textStart page_margin grid_rise
           mapM_ go $ map showLineItems $ dOrganizations d
         beginPath (300 :+ 300)
         lineto (350 :+ 320)
         strokePath
         closePath
         drawText $ text font_normal 0.0 0.0 $ toPDFString "!"
