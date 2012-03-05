
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
{-# LANGUAGE OverloadedStrings #-}

module Name
    ( Name
    , Strategy (render, _compare)
    , sur
    , given
    , mkName
    , firstLast
    , lastFirst
    ) where

import Control.Applicative
import Data.Function
import Data.Monoid

import Data.Aeson ((.=), (.:))
import qualified Data.Aeson as A
import Data.Text (Text)
import qualified Data.Text as T

-- |A contact's name. This sorts by last name and prints out as 'Last,
-- First'.
data Name
  = FirstLast {-# UNPACK #-} !Text {-# UNPACK #-} !Text
  -- |Use this whenever you want a contact to always sort and print the
  -- same way.
  | SingleName {-# UNPACK #-} !Text
  deriving (Eq)

data Strategy = Strategy
    { render   :: Name -> Text
    , _compare :: Name -> Name -> Ordering }

given :: Name -> Text
given (FirstLast f _) = f
given (SingleName n) = n

sur :: Name -> Maybe Text
sur (FirstLast _ l) = Just l
sur _ = Nothing

firstLast, lastFirst :: Strategy
firstLast = Strategy { render = renderFL, _compare = compareFL }
lastFirst = Strategy { render = renderLF, _compare = compareLF }

renderFL :: Name -> Text
renderFL (FirstLast f l) = f `T.append` " " `T.append` l
renderFL (SingleName  n) = n

renderLF :: Name -> Text
renderLF (FirstLast f l) = l `T.append` ", " `T.append` f
renderLF (SingleName  n) = n

compareFL :: Name -> Name -> Ordering
compareFL (FirstLast a b) (FirstLast c d) = compare a c <> compare b d
compareFL (FirstLast a b) (SingleName c) = compare (T.append a b) c
compareFL (SingleName c) (FirstLast a b) = compare c (T.append a b)
compareFL (SingleName a) (SingleName b) = compare a b

compareLF :: Name -> Name -> Ordering
compareLF = compareFL `on` swapFL

swapFL :: Name -> Name
swapFL (FirstLast f l) = FirstLast l f
swapFL n = n

instance A.ToJSON Name where
  toJSON n = case n of
      SingleName sn -> A.toJSON sn
      _ -> A.object
          [ "first" .= A.toJSON (given n)
          , "last"  .= A.toJSON (sur n)]

instance A.FromJSON Name where
  parseJSON (A.Object v) = FirstLast
      <$> v .: "first"
      <*> v .: "last"
  parseJSON sn = SingleName <$> A.parseJSON sn

-- |Convencience function to create a name from two strings.
mkName :: Text -- ^First name or blank.
       -> Text -- ^Last name or blank.
       -> Name
mkName f l  | T.null f  = SingleName l
            | T.null l  = SingleName f
            | otherwise = FirstLast f l
