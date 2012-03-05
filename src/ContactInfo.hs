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

module ContactInfo
    ( ContactInfo (..)
    , compareCI
    , renderWith
    , toCSVRecord
    , toLineItem
    ) where

import Control.Applicative
import Data.Function
import Data.Maybe (fromMaybe)
import Control.Monad (mzero)
import Data.Monoid

import Data.Aeson ((.=), (.:))
import qualified Data.Aeson as A
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Text.CSV.ByteString (Record)

import LineItem
import Name as N
import Priority

-- |Contact information for an individual or group.
data ContactInfo = ContactInfo
  { -- |for the purposes of sorting.  Higher numbers sort first.
    cPriority :: {-# UNPACK #-} !Priority
  , -- |Either a Name or FirstSortedName.  This ends up on the left
    -- side of each line item.
    cName :: !N.Name
    -- |A phone number.  This ends up on the right of each line item.
  , cPhone    :: {-# UNPACK #-} !Text
  } deriving (Eq)

compareCI :: N.Strategy
    -> ContactInfo -> ContactInfo -> Ordering
compareCI ns l r = cPriority l `compare` cPriority r
    <> (N._compare ns `on` cName) l r
    <> cPhone l `compare` cPhone r

renderWith :: N.Strategy -> ContactInfo -> Text
renderWith ns = N.render ns . cName

instance A.ToJSON ContactInfo where
  toJSON ci = A.object
      [ "name"     .= A.toJSON (cName ci)
      , "phone"    .= A.toJSON (cPhone ci)
      , "priority" .= A.toJSON (cPriority ci)]

instance A.FromJSON ContactInfo where
  parseJSON (A.Object v) = ContactInfo
      <$> v .: "priority"
      <*> v .: "name"
      <*> v .: "phone"
  parseJSON _ = mzero

toLineItem :: N.Strategy -> ContactInfo -> LineItem
toLineItem s ci = mkLabelValue (T.unpack . renderWith s $ ci)
    (T.unpack . cPhone $ ci)

toCSVRecord :: ContactInfo -> Record
toCSVRecord (ContactInfo priority n phone) = map T.encodeUtf8
    [ N.given n
    , fromMaybe "" $ N.sur n
    , phone
    , T.pack . show . toInt $ priority ]
