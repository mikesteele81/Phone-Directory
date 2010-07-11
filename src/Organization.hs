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

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Organization where

import qualified Data.ByteString.Char8 as B
import Data.Convertible.Base
import Data.List (sort)
import Data.Object
import Data.Object.Json
import Text.CSV (Record)

import qualified ContactInfo as C
import LineItem

-- |An organization has its own contact information and 0 or more
-- contacts that are a part of it.
data Organization c = Organization
  { -- |Contact information for the organization itself.  This gets
    -- turned into a non-indented line item.
    oInfo :: c
    -- |Contacts that make up the organization.  These all get turned
    -- into indented line items on the contact sheet.
  , oContacts :: [c]
  } deriving (Eq, Ord, Show)

instance (ConvertAttempt JsonObject a)
    => ConvertAttempt JsonObject (Organization a) where
  convertAttempt j =
      do m <- fromMapping j
         i <- lookupObject (B.pack "info") m >>= convertAttempt
         cx <- lookupSequence (B.pack "contacts") m >>= mapM convertAttempt
         return $ Organization i cx

instance (ConvertSuccess a JsonObject)
    => ConvertSuccess (Organization a) JsonObject where
  convertSuccess (Organization i cx) =
      Mapping [ (B.pack "info", convertSuccess i)
              , (B.pack "contacts", Sequence $ map convertSuccess cx)]

instance (ShowLineItems a) => ShowLineItems (Organization a) where
    showLineItems (Organization o cx) = concat $ [header] : map showLineItems cx
        where header = (head $ showLineItems o) {indent = False}

-- |Sort all the contacts.
sortOrg :: (Ord a) => Organization a -> Organization a
sortOrg o = o { oContacts = sort (oContacts o) }

-- Perform an operation on the name.  Is this an abuse of Functors?
instance Functor Organization where
    f `fmap` o = o { oInfo = f $ oInfo o, oContacts = map f $ oContacts o}

toCSVRecords :: Show a => Organization (C.ContactInfo a) -> [Record]
toCSVRecords (Organization i cx) = map (C.toCSVRecord ir) cx
  where
    ir = C.toCSVRecord [] i