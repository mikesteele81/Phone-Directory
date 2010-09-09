
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

module Name
    ( Name (FirstLast, SingleName)
    , sur
    , given
    , mkName
    , toFirstSorted
    ) where

import Control.Applicative
import Data.Attempt
import Data.ByteString.Char8
import Data.Function (on)
import Data.Convertible.Base
import Data.Object
import qualified Data.Object.Json as J

-- |A contact's name. This sorts by last name and prints out as 'Last,
-- First'.
data Name
  = FirstLast { given :: String
              , sur :: String }
  | LastFirst { sur :: String
              , given :: String }
  -- |Use this whenever you want a contact to always sort and print the
  -- same way.
  | SingleName String
  deriving (Eq, Ord)

instance Show Name where
  show (FirstLast f l) = f ++ " " ++ l
  show (LastFirst l f) = l ++ ", " ++ f
  show (SingleName n)  = n

-- |Convencience function to create a name from two strings.
mkName :: String -- ^First name or blank.
       -> String -- ^Last name or blank.
       -> Name
mkName "" n = SingleName n
mkName n "" = SingleName n
mkName f l  = LastFirst l f

instance ConvertAttempt J.JsonObject Name where
  convertAttempt j =
      maybe (Failure NothingException) Success $
      fromAttempt ( do m <- fromMapping j
                       f <- lookupScalar (pack "first") m
                       l <- lookupScalar (pack "last") m
                       return $ (mkName `on` J.fromJsonScalar) f l
                  )
      <|> fromAttempt (SingleName. J.fromJsonScalar <$> fromScalar j)

instance ConvertSuccess Name J.JsonObject where
    convertSuccess (SingleName n) = (Scalar . J.JsonString . pack) n
    convertSuccess n  =
      J.toJsonObject $ Mapping
          [ ("first", Scalar . given $ n), ("last", Scalar . sur $ n)]

toFirstSorted :: Name -> Name
toFirstSorted (LastFirst l f) = FirstLast f l
toFirstSorted n = n
