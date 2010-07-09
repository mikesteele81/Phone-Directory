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
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module PageProperties
    ( PageProperties (..)
    , mkPageProperties
    ) where

import Data.Attempt
import qualified Data.ByteString.Char8 as B
import Data.Convertible.Base
import Data.Object
import qualified Data.Object.Json as J

import UnitConversion

data PageProperties = PageProperties
    { pageWidth    :: Inches
    , pageHeight   :: Inches
    , leftMargin   :: Inches
    , rightMargin  :: Inches
    , topMargin    :: Inches
    , bottomMargin :: Inches }
    deriving (Eq, Show)

instance ConvertAttempt J.JsonObject PageProperties where
  convertAttempt j =
      do m <- fromMapping j
         pw <- attempt (\_ -> return $ Inches 8.5) convertAttempt 
               (lookupScalar (B.pack "pageWidth") m)
         ph <- attempt (\_ -> return $ Inches 11.0) convertAttempt
               (lookupScalar (B.pack "pageHeight") m)
         lm <- attempt (\_ -> return quarterInch) convertAttempt
               (lookupScalar (B.pack "leftMargin") m)
         rm <- attempt (\_ -> return quarterInch) convertAttempt
               (lookupScalar (B.pack "rightMargin") m)
         tm <- attempt (\_ -> return quarterInch) convertAttempt
               (lookupScalar (B.pack "topMargin") m)
         bm <- attempt (\_ -> return quarterInch) convertAttempt
               (lookupScalar (B.pack "bottomMargin") m)
         return $ PageProperties pw ph lm rm tm bm

instance ConvertSuccess PageProperties J.JsonObject where
  convertSuccess (PageProperties pw ph lm rm tm bm) =
      Mapping $ [ (B.pack "pageWidth"   , Scalar $ convertSuccess pw)
                , (B.pack "pageHeight"  , Scalar $ convertSuccess ph)
                , (B.pack "leftMargin"  , Scalar $ convertSuccess lm)
                , (B.pack "rightMargin" , Scalar $ convertSuccess rm)
                , (B.pack "topMargin"   , Scalar $ convertSuccess tm)
                , (B.pack "bottomMargin", Scalar $ convertSuccess bm)]

mkPageProperties :: PageProperties
mkPageProperties = PageProperties
    { pageWidth    = Inches 8.5
    , pageHeight   = Inches 11.0
    , leftMargin   = quarterInch
    , rightMargin  = quarterInch
    , topMargin    = quarterInch
    , bottomMargin = quarterInch }

quarterInch :: Inches
quarterInch = Inches 0.25