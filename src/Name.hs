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

module Name
    ( Name            ( FirstLast, SingleName)
    , FirstSortedName ( FirstSortedName )
    ) where

import Control.Applicative
import Data.Char           ( toLower )
import Text.JSON
import Text.JSON.Pretty    ( pp_value )

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
newtype FirstSortedName = FirstSortedName { unFirstSortedName :: Name }
    deriving (Eq)

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

-- |This class is a hack to simplify the sorting of names.
class ShowForSorting a where
    showForSorting :: a -> String
    
instance ShowForSorting FirstSortedName where
    showForSorting (FirstSortedName n) =
        case n of
          FirstLast f l -> map toLower $ f ++ l
          _             -> showForSorting n
          
instance ShowForSorting Name where
    showForSorting n =
        case n of
          FirstLast f l -> map toLower $ l ++ f
          SingleName sn -> map toLower sn

instance Ord Name where
    compare l r = compare (showForSorting l) (showForSorting r)

instance Ord FirstSortedName where
    compare l r = compare (showForSorting l) (showForSorting r)
    
instance JSON Name where
    readJSON v@(JSObject o) =
        (FirstLast <$> valFromObj "first" o <*> valFromObj "last" o)
        <|> (Error $ "Unable to parse name: " ++ (show . pp_value) v)
    readJSON (JSString s) = return . SingleName $ fromJSString s
    readJSON v = Error $ "Unable to parse Name: " ++ (show . pp_value) v
    showJSON n = case n of
        FirstLast f l -> makeObj [ ("first", showJSON f), ("last" , showJSON l) ]
        SingleName sn -> showJSON sn
                   
instance JSON FirstSortedName where
    readJSON n = FirstSortedName <$> readJSON n
    showJSON   = showJSON . unFirstSortedName
