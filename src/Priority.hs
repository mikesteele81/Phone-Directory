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

module Priority
    ( Priority ()
    , mkPriority
    ) where

import Control.Applicative
import Control.Monad
import Text.JSON
import Text.JSON.Pretty (pp_value)

newtype Priority = Priority { unP :: Int }
    -- the Show instance is only for QC properties
    deriving (Enum, Eq, Ord, Show)

instance Bounded Priority where
    minBound = Priority 0
    maxBound = Priority 5

instance JSON Priority where
  readJSON x = liftM mkPriority (readJSON x)
      <|> Error ("Unable to parse priority: " ++ (show . pp_value) x)
  showJSON = showJSON . unP

-- |Makes a Priority from an int, automatically clipping the results to be
-- within range.
mkPriority :: Int -> Priority
mkPriority = min maxBound . max minBound . Priority
