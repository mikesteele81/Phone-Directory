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
import Data.Attempt
import Data.Convertible.Base
import Data.Function(on)
import Data.List
import Data.Maybe
import Data.Object
import Data.Object.Json
import Text.CSV

import qualified ContactInfo as C
import LineItem
import Name
import Priority

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

toLineItems :: Show a => Organization (C.ContactInfo a) -> [LineItem]
toLineItems (Organization o cx) = intersperse Indent . map C.toLineItem $ o : cx

-- |Sort all the contacts.
sortOrg :: (Ord a) => Organization a -> Organization a
sortOrg o = o { oContacts = sort (oContacts o) }

-- Perform an operation on the name.  Is this an abuse of Functors?
instance Functor Organization where
    f `fmap` o = o { oInfo = f $ oInfo o, oContacts = map f $ oContacts o}

toCSV :: Show a => Organization (C.ContactInfo a) -> CSV
toCSV (Organization i []) = return $ C.toCSVRecord i ++ ["", ""]
toCSV (Organization i cx) = map (\c -> ir ++ C.toCSVRecord c) cx
  where
    ir = C.toCSVRecord i

fromCSV :: CSV -> Attempt [Organization (C.ContactInfo Name)]
fromCSV = sequence . map fromCSVRecord
          -- the 'csv' package parses an empty line as a blank record.
          . takeWhile (/=[""])

fromCSVRecord :: Record -> Attempt (Organization (C.ContactInfo Name))
fromCSVRecord [orgN, op, cn, cp] = Success $
    Organization (C.ContactInfo pr (mkName orgN "") op)
    [C.ContactInfo pr (mkName cn "") cp]
  where pr = mkPriority 2
fromCSVRecord r = Failure . StringException
                  $ "Expected record of 4 fields: " ++ show r

-- |Attempt to merge every member of the list together.
mergeOrgs :: Eq a => [Organization a] -> [Organization a]
mergeOrgs ox = foldl (flip mergeOrg) [] ox

-- |Attempt to merge the first argument with one of the elements in the 
--  second argument. If this isn't possible, add the first argument to the 
--  head of the list.
mergeOrg :: Eq a => Organization a -> [Organization a] -> [Organization a]
mergeOrg o ox = if isNothing found then ox' else o:ox'
  where
    (found, ox') = mapAccumL mergeOp (Just o) ox

-- |Possibly merge two organizations together. If a merge is made return 
--  Nothing tupled with the merged Organization.  If a merge is not 
--  possible, return the original two arguments tupled.
mergeOp :: Eq a => Maybe (Organization a) -> Organization a
    -> (Maybe (Organization a), Organization a)
mergeOp (Just o) x
    | oInfo o == oInfo x = ( Nothing
                           , x {oContacts = ((++) `on` oContacts) o x})
    | otherwise = (Just o, x)
mergeOp Nothing x = (Nothing, x)
