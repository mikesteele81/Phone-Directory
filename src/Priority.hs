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

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Priority
    ( Priority ()
    , mkPriority
    ) where

import Control.Applicative
import Data.Attempt
import Data.Convertible.Base
import Data.Object
import Data.Object.Json

newtype Priority = Priority Int
    -- the Show instance is only for QC properties
    deriving (Enum, Eq, Ord, Show)

instance Bounded Priority where
    minBound = Priority 0
    maxBound = Priority 5

-- |Makes a Priority from an int, automatically clipping the results to be
-- within range.
mkPriority :: Int -> Priority
mkPriority = min maxBound . max minBound . Priority

instance ConvertSuccess Priority JsonObject where
  convertSuccess (Priority p) = return . JsonNumber . convertSuccess $ p

instance ConvertAttempt JsonObject Priority where
  convertAttempt (Scalar (JsonNumber r))  =
      mkPriority <$> convertAttempt r
  convertAttempt _ = failure NothingException
