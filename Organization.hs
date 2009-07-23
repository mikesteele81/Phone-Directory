module Organization where

import Data.List (sort)
import Graphics.PDF
import Text.JSON

import Constants
import ContactInfo

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

sortOrg :: (Ord a) => Organization a -> Organization a
sortOrg o =
    o { oContacts = sort (oContacts o) }

-- | Draw an Organization.  Supply the x and y of the upper left corner.
-- Returns (w, h)
drawOrg :: (Show a) => Organization a -> PDFText ()
drawOrg o = do
  drawCI (oInfo o) line_item_width
  startNewLine
  textStart line_item_indent 0
  mapM_ (\x -> do
           drawCI x (line_item_width - line_item_indent)
           startNewLine) $ oContacts o
  textStart (0 - line_item_indent) 0