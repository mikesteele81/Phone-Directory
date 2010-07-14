
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

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Name
    ( Name            ( FirstLast, SingleName)
    , FirstSortedName ( FirstSortedName )
    , mkName
    ) where

import Control.Applicative
import Data.Attempt
import Data.ByteString.Char8
import Data.Function (on)
import Data.Monoid
import Data.Convertible.Base
import Data.Object
import qualified Data.Object.Json as J

-- |A contact's name. This sorts by last name and prints out as 'Last,
-- First'.
data Name
  -- |FirstLast first last
  = FirstLast String String
  -- |Use this whenever you want a contact to always sort and print the
  -- same way.
  | SingleName String
  deriving (Eq)

-- |This prints out 'First Last' and sorts by first name.
newtype FirstSortedName = FirstSortedName { unFSN :: Name }
    deriving (Eq)

instance Show FirstSortedName where
  show (unFSN -> FirstLast f l) = f ++ " " ++ l
  -- It must be a SingleName
  show (unFSN -> n)             = show n
    
instance Show Name where
  show (FirstLast f l) = l ++ ", " ++ f
  show (SingleName n)  = n

instance Ord Name where
    compare (FirstLast fl ll) (FirstLast fr lr) =
       compare ll lr `mappend` compare fl fr
    -- Bug? why are we ignoring the first name in comparisons?
    compare (FirstLast _ l) (SingleName n) = compare l n `mappend` GT
    compare (SingleName n) (FirstLast _ l) = compare n l `mappend` LT
    compare (SingleName l) (SingleName r) = compare l r

instance Ord FirstSortedName where
  compare (unFSN -> FirstLast fl ll) (unFSN -> FirstLast fr lr) =
      compare fl fr `mappend` compare ll lr
  compare (unFSN -> FirstLast f _) (unFSN -> SingleName n) =
      compare f n `mappend` GT
  compare (unFSN -> SingleName n) (unFSN -> FirstLast f _) =
      compare n f `mappend` LT
  -- The only other possibility is SingleName to SingleName
  compare l r = (compare `on` unFSN) l r

-- |Convencience function to create a name from two strings.
mkName :: String -- ^First name or blank.
       -> String -- ^Last name or blank.
       -> Name
mkName "" n = SingleName n
mkName n "" = SingleName n
mkName f l  = FirstLast f l

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
  convertSuccess (FirstLast f l) =
      J.toJsonObject $ Mapping
          [ ("first", Scalar f), ("last", Scalar l)]
  convertSuccess (SingleName n) = (Scalar . J.JsonString . pack) n

