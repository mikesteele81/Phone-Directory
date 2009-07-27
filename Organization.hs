{-# LANGUAGE ExistentialQuantification #-}
module Organization where

import Control.Applicative
import Data.List (sort)
import Graphics.PDF
import Text.JSON

import Constants
import ContactInfo
import LineItem

-- An organization persons are a part of
data Organization name
    = Organization
      { oInfo :: ContactInfo name
      , oContacts :: [ContactInfo name]
      } deriving (Eq)

instance (JSON a) => JSON (Organization a) where
    readJSON (JSObject o) =
        do
          info <- valFromObj "info" o
          contacts <- valFromObj "contacts" o
          return $ Organization info contacts
    readJSON _ = Error "Could not parse Organization JSON object."
    showJSON o =
        showJSON $ toJSObject $
                 [ ("info", showJSON $ oInfo o)
                 , ("contacts", showJSONs $ oContacts o)]
                 
instance (Ord a) => Ord (Organization a) where
    compare l r = compare (oInfo l) (oInfo r)
    
instance forall a. (Show a) => ShowLineItems (Organization a) where
    showLineItems (Organization o cx) =
        let rest = getZipList $ mkLabelValue True 
                   <$> ZipList (map cName cx)
                   <*> ZipList (map cPhone cx)
        in mkLabelValue False (show $ cName o) (cPhone o) : rest

sortOrg :: (Ord a) => Organization a -> Organization a
sortOrg o =
    o { oContacts = sort (oContacts o) }

-- | Draw an Organization.  Supply the x and y of the upper left corner.
-- Returns (w, h)
drawOrg :: (Show a) => Organization a -> PDFText ()
drawOrg o =
    let lx = showLineItems o
        op l = drawLineItem line_item_width l >> startNewLine
    in do
      leading line_item_leading
      -- draw each contact
      mapM_ op lx
