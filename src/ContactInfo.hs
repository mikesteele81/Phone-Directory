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
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module ContactInfo
    ( ContactInfo (..)
    ) where

import Control.Applicative
import Data.Attempt ()
import Data.ByteString.Char8
import Data.Convertible.Base
import Data.Object
import qualified Data.Object.Json as J

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

instance forall a. (Show a) => Show (ContactInfo a) where
    show = show . cName

-- Perform an operation on the name.  Is this an abuse of Functors?
instance Functor ContactInfo where
    f `fmap` x = x { cName = (f . cName) x }

instance (Show a) => ShowLineItems (ContactInfo a) where
    showLineItems ci = [mkLabelValue True (show $ cName ci) (cPhone ci)]

instance (ConvertAttempt J.JsonObject a)
    => ConvertAttempt J.JsonObject (ContactInfo a) where
  convertAttempt j =
      do m <- fromMapping j
         pr <- lookupObject (pack "priority") m >>= convertAttempt
         n  <- lookupObject (pack "name")     m >>= convertAttempt
         p  <- J.fromJsonScalar <$> lookupScalar (pack "phone") m
         return $ ContactInfo pr n p

instance (ConvertSuccess a J.JsonObject)
    => ConvertSuccess (ContactInfo a) J.JsonObject where
  convertSuccess (ContactInfo pr n p) =
      Mapping [ (pack "name", convertSuccess n)
              , (pack "phone", Scalar $ J.toJsonScalar p)
              , (pack "priority", convertSuccess pr)]
