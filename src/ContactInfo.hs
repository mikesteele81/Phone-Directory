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
module ContactInfo
    ( ContactInfo (..)
    ) where

import Control.Applicative
import Text.JSON
import Text.JSON.Pretty

-- |Contact information for an individual or group.
data ContactInfo a = ContactInfo
  { -- |for the purposes of sorting.  Higher numbers sort first.
    cPriority :: Int
  , -- |Either a Name or FirstSortedName.  This ends up on the left
    -- side of each line item.
    cName :: a
    -- |A phone number.  This ends up on the right of each line item.
  , cPhone    :: String
  } deriving (Eq, Ord)

instance (JSON a) => JSON (ContactInfo a) where
    readJSON v@(JSObject o)
        =   ContactInfo <$> valFromObj "priority" o <*> valFromObj "name" o
        <*> valFromObj "phone" o
        <|> Error ("Failed to parse contact information from "
            ++ (show . pp_value) v ++ ".")
    readJSON v = Error $ "Expected JSObject, but " ++ (show . pp_value) v
        ++ " found while parsing a contact information."
    showJSON (ContactInfo n p pr) =
       makeObj [ ("name", showJSON n), ("phone", showJSON p)
               , ("priority", showJSON pr) ]

instance forall a. (Show a) => Show (ContactInfo a) where
    show = show . cName

-- Perform an operation on the name.  Is this an abuse of Functors?
instance Functor ContactInfo where
    f `fmap` x = x { cName = (f . cName) x }
