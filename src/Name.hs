
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

{-# LANGUAGE GeneralizedNewtypeDeriving, ViewPatterns #-}

module Name
    ( Name            ( FirstLast, SingleName)
    , FirstSortedName ( FirstSortedName )
    , mkName
    ) where

import Control.Applicative
import Data.Function (on)
import Data.Monoid
import Text.JSON
import Text.JSON.Pretty (pp_value)

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
    deriving (Eq, JSON)

instance Show FirstSortedName where
    show (FirstSortedName n) =
        case n of
          FirstLast f l -> f ++ " " ++ l
          SingleName sn -> sn
    
instance Show Name where
    show n =
        case n of
          FirstLast f l -> l ++ ", " ++ f
          SingleName sn -> sn

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

instance JSON Name where
    readJSON v@(JSObject o) =
        (FirstLast <$> valFromObj "first" o <*> valFromObj "last" o)
        <|> (Error $ "Unable to parse name: " ++ (show . pp_value) v)
    readJSON (JSString s) = return . SingleName $ fromJSString s
    readJSON v = Error $ "Unable to parse Name: " ++ (show . pp_value) v
    showJSON n = case n of
        FirstLast f l -> makeObj [ ("first", showJSON f), ("last" , showJSON l) ]
        SingleName sn -> showJSON sn
                   
-- |Convencience function to create a name from two strings. This lets us pull
-- user input directly from the GUI
mkName :: String -- ^First name or blank.
    -> String    -- ^Last name or blank.
    -> Name
mkName f l =
    case (f, l) of
        ("", n) -> SingleName n
        (n, "") -> SingleName n
        _       -> FirstLast f l
