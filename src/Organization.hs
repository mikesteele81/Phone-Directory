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
data Organization = Organization
  { -- |Contact information for the organization itself.  This gets
    -- turned into a non-indented line item.
    oInfo :: C.ContactInfo
    -- |Contacts that make up the organization.  These all get turned
    -- into indented line items on the contact sheet.
  , oContacts :: [C.ContactInfo]
  } deriving (Eq, Ord, Show)

instance ConvertAttempt JsonObject Organization where
  convertAttempt j =
      do m <- fromMapping j
         i <- lookupObject (B.pack "info") m >>= convertAttempt
         cx <- lookupSequence (B.pack "contacts") m >>= mapM convertAttempt
         return $ Organization i cx

instance ConvertSuccess Organization JsonObject where
  convertSuccess (Organization i cx) =
      Mapping [ (B.pack "info", convertSuccess i)
              , (B.pack "contacts", Sequence $ map convertSuccess cx)]

toFirstSorted :: Organization -> Organization
toFirstSorted (Organization i cx) =
    Organization (C.toFirstSorted i) (map C.toFirstSorted cx)

toLineItems :: Organization -> [LineItem]
toLineItems (Organization o cx) = intersperse Indent . map C.toLineItem $ o : cx

-- |Sort all the contacts.
sortOrg :: Organization -> Organization
sortOrg o = o { oContacts = sort (oContacts o) }

toCSV :: Organization -> CSV
toCSV (Organization i []) = return $ C.toCSVRecord i ++ ["", "", "", "", "", ""]
toCSV (Organization i cx) = map (\c -> ir ++ C.toCSVRecord c) cx
  where
    ir = C.toCSVRecord i

fromCSV :: CSV -> Attempt [Organization]
fromCSV = sequence . map fromCSVRecord
          -- the 'csv' package parses an empty line as a blank record.
          . takeWhile (/=[""])

fromCSVRecord :: Record -> Attempt Organization
fromCSVRecord [ oFn, oLn, oPhone, oPriority, _, _
              , cFn, cLn, cPhone, cPriority, _, _] = Success $
    --read can fail here.
    Organization (C.ContactInfo (mkPriority . read $ oPriority)
        (mkName oFn oLn) oPhone)
    [C.ContactInfo (mkPriority . read $ cPriority) (mkName cFn cLn) cPhone]
fromCSVRecord r = Failure . StringException
                  $ "Expected record of 12 fields: " ++ show r

-- |Attempt to merge every member of the list together.
mergeOrgs :: [Organization] -> [Organization]
mergeOrgs ox = foldl (flip mergeOrg) [] ox

-- |Attempt to merge the first argument with one of the elements in the 
--  second argument. If this isn't possible, add the first argument to the 
--  head of the list.
mergeOrg :: Organization -> [Organization] -> [Organization]
mergeOrg o ox = if isNothing found then ox' else o:ox'
  where
    (found, ox') = mapAccumL mergeOp (Just o) ox

-- |Possibly merge two organizations together. If a merge is made return 
--  Nothing tupled with the merged Organization.  If a merge is not 
--  possible, return the original two arguments tupled.
mergeOp :: Maybe Organization -> Organization -> (Maybe Organization, Organization)
mergeOp (Just o) x
    | oInfo o == oInfo x = ( Nothing
                           , x {oContacts = ((++) `on` oContacts) o x})
    | otherwise = (Just o, x)
mergeOp Nothing x = (Nothing, x)
