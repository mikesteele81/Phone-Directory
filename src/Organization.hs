{- This file is part of PhoneDirectory.

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
module Organization where

import Control.Applicative
import Data.List (sort)
import Text.JSON

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
                   <$> ZipList (map (show . cName) cx)
                   <*> ZipList (map cPhone cx)
        in mkLabelValue False (show $ cName o) (cPhone o) : rest

sortOrg :: (Ord a) => Organization a -> Organization a
sortOrg o = o { oContacts = sort (oContacts o) }

-- Perform an operation on the name.  Is this an abuse of Functors?
instance Functor Organization where
    f `fmap` o = o { oInfo = fmap f $ oInfo o
                   , oContacts = map (fmap f) $ oContacts o
                   } 