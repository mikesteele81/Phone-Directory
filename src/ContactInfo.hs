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

module ContactInfo
    ( ContactInfo (..)
    , toCSVRecord
    , toFirstSorted
    , toLineItem
    ) where

import Control.Applicative
import Data.Attempt ()
import Data.ByteString.Char8
import Data.Convertible.Base
import Data.Object
import qualified Data.Object.Json as J
import Text.CSV (Record)

import LineItem
import qualified Name as N
import Priority

-- |Contact information for an individual or group.
data ContactInfo = ContactInfo
  { -- |for the purposes of sorting.  Higher numbers sort first.
    cPriority :: Priority
  , -- |Either a Name or FirstSortedName.  This ends up on the left
    -- side of each line item.
    cName :: N.Name
    -- |A phone number.  This ends up on the right of each line item.
  , cPhone    :: String
  } deriving (Eq, Ord)

instance Show ContactInfo where
    show = show . cName

toFirstSorted :: ContactInfo -> ContactInfo
toFirstSorted c@(ContactInfo _ n _) = c { cName = N.toFirstSorted n}

toLineItem :: ContactInfo -> LineItem
toLineItem ci = mkLabelValue (show $ cName ci) (cPhone ci)

instance ConvertAttempt J.JsonObject ContactInfo where
  convertAttempt j =
      do m <- fromMapping j
         pr <- lookupObject (pack "priority") m >>= convertAttempt
         n  <- lookupObject (pack "name")     m >>= convertAttempt
         p  <- J.fromJsonScalar <$> lookupScalar (pack "phone") m
         return $ ContactInfo pr n p

instance ConvertSuccess ContactInfo J.JsonObject where
  convertSuccess (ContactInfo pr n p) =
      Mapping [ (pack "name", convertSuccess n)
              , (pack "phone", Scalar $ J.toJsonScalar p)
              , (pack "priority", convertSuccess pr)]

toCSVRecord :: ContactInfo -> Record
toCSVRecord (ContactInfo priority n phone) =
    [ given
    , sur
    , phone
    , show . toInt $ priority
    , show . N.toFirstSorted $ n
    , show . N.toLastSorted  $ n ]
  where
    (given, sur) = case n of
      N.SingleName x -> (x, "")
      _ -> (N.given n, N.sur n)
