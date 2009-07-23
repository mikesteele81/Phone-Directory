{-# LANGUAGE ExistentialQuantification #-}
module ContactInfo
    ( ContactInfo (..)
    , drawCI
    ) where

import Control.Applicative
import Data.Monoid
import Graphics.PDF
import Text.JSON

import Constants
    
data ContactInfo name = ContactInfo
    { cName :: name
    , cPhone    :: String
    -- for the purposes of sorting.  Higher numbers sort first.
    , cPriority :: Int
    } deriving (Eq)
    
instance (JSON a) => JSON (ContactInfo a) where
    readJSON (JSObject o) =
        ContactInfo <$> valFromObj "name" o <*> valFromObj "phone" o
        <*> valFromObj "priority" o
    readJSON _ =
        Error "Could not parse ContactInfo JSON object."
    showJSON (ContactInfo n p pr) =
       makeObj [ ("name", showJSON n), ("phone", showJSON p)
               , ("priority", showJSON pr) ]

instance forall a. (Ord a) => Ord (ContactInfo a) where
    compare l r =                     
        -- priority descending
        compare (cPriority r) (cPriority l) `mappend`
        compare (cName l) (cName r) `mappend`
        compare (cPhone l) (cPhone r)

-- Perform an operation on the name.  Is this an abuse of Functors?
instance Functor ContactInfo where
    f `fmap` x = x { cName = (f . cName) x }

-- | Draw a ContactInfo.  Return the width and height
drawCI :: (Show a) => ContactInfo a -> PDFFloat -> PDFFloat
       -> Draw (PDFFloat, PDFFloat)
drawCI ci x y =
    let name = (toPDFString . show . cName) ci
        phone = (toPDFString . cPhone) ci
    in do drawText $ text font_normal x y name
          drawText $ text font_normal
                       (x + line_item_width - textWidth font_normal phone)
                       y phone
          return (line_item_width, getHeight font_normal)
