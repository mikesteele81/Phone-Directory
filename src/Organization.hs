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

{-# LANGUAGE OverloadedStrings #-}

module Organization where

import Control.Applicative
import Control.Monad (mzero)
import Data.Function(on)
import Data.List
import Data.Maybe

import Data.Aeson ((.=), (.:))
import qualified Data.Aeson as A
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.Text.Read as T
import Text.CSV.ByteString

import qualified ContactInfo as C
import LineItem
import Name
import Priority

-- |An organization has its own contact information and 0 or more
-- contacts that are a part of it.
data Organization = Organization
  { -- |Contact information for the organization itself.  This gets
    -- turned into a non-indented line item.
    oInfo :: !C.ContactInfo
    -- |Contacts that make up the organization.  These all get turned
    -- into indented line items on the contact sheet.
  , oContacts :: [C.ContactInfo]
  } deriving (Eq)

toLineItems :: Strategy -> Organization -> [LineItem]
toLineItems s (Organization o cx) = intersperse Indent . map (C.toLineItem s)
    $ o : cx

instance A.ToJSON Organization where
  toJSON o = A.object
      [ "info" .= A.toJSON (oInfo o)
      , "contacts" .= A.toJSON (oContacts o)]

instance A.FromJSON Organization where
  parseJSON (A.Object v) = Organization
      <$> v .: "info"
      <*> v .: "contacts"
  parseJSON _ = mzero

-- |Sort all the contacts.
sortOrg :: Strategy -> Organization -> Organization
sortOrg ns o = o { oContacts = sortBy (C.compareCI ns) (oContacts o) }

toCSV :: Organization -> CSV
toCSV (Organization i []) = return $ C.toCSVRecord i ++ ["", "", "", "1"]
toCSV (Organization i cx) = map (\c -> ir ++ C.toCSVRecord c) cx
  where
    ir = C.toCSVRecord i

fromCSV :: CSV -> Either Text [Organization]
fromCSV = mapM fromCSVRecord

toTextRecord :: Record -> Either T.UnicodeException [Text]
toTextRecord = mapM T.decodeUtf8'

fromCSVRecord :: Record -> Either Text Organization
fromCSVRecord fs = do
    (oFs, cFs) <- either (\e -> Left $ "Unable to decode field: "
                         `T.append` T.pack (show e))
                  (Right . splitAt 4) $ toTextRecord fs
    o <- fieldsToContactInfo oFs
    c <- fieldsToContactInfo cFs
    return $ Organization o [c]

fieldsToContactInfo :: [Text] -> Either Text C.ContactInfo
fieldsToContactInfo [f, l, p, pr] = do
    pr' <- either (\e -> Left $ "Unable to decode priority field: "
                         `T.append` T.pack (show e))
           (Right . mkPriority . fst) $ T.decimal pr
    return $ C.ContactInfo pr' (mkName f l) p
fieldsToContactInfo _
    = Left "Expected 4 fields to construct contact information with."

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
