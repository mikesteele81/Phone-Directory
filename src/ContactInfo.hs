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
import Control.Monad.Error
import Text.JSON
import Text.JSON.Pretty

import LineItem
import Priority

-- |Contact information for an individual or group.
data ContactInfo a = ContactInfo
  { -- |for the purposes of sorting.  Higher numbers sort first.
    cPriority :: Priority
  , -- |Either a Name or FirstSortedName.  This ends up on the left
    -- side of each line item.
    cName :: a
    -- |A phone number.  This ends up on the right of each line item.
  , cPhone    :: String
  } deriving (Eq, Ord)

instance (JSON a) => JSON (ContactInfo a) where
    readJSON (JSObject o) =
        ( ContactInfo <$> valFromObj "priority" o
          <*> valFromObj "name" o
          <*> (valFromObj "phone" o
-- Error handling here uses catchError, and then displays the real JSON error
-- message. Elsewhere I'm doing things differently. Which is best?          
               `catchError` (\e -> Error $ "failed on phone: " ++ e)))
        `catchError` (\e -> Error $ msg e)
      where
        msg e = "Failed to parse contact information: " ++ e
    readJSON v = Error $ "Expected JSObject, but " ++ (show . pp_value) v
        ++ " found while parsing a contact information."
    showJSON ci = makeObj
        [ ("name", showJSON . cName $ ci), ("phone", showJSON . cPhone $ ci)
        , ("priority", showJSON . cPriority $ ci) ]

instance forall a. (Show a) => Show (ContactInfo a) where
    show = show . cName

-- Perform an operation on the name.  Is this an abuse of Functors?
instance Functor ContactInfo where
    f `fmap` x = x { cName = (f . cName) x }

instance (Show a) => ShowLineItems (ContactInfo a) where
    showLineItems ci = [mkLabelValue True (show $ cName ci) (cPhone ci)]
