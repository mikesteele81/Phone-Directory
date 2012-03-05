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

{-# LANGUAGE OverloadedStrings #-}

module PageProperties
    ( PageProperties (..)
    , mkPageProperties
    ) where

import Control.Applicative
import Control.Monad (mzero)

import Data.Aeson ((.=), (.:))
import qualified Data.Aeson as A

import UnitConversion

data PageProperties = PageProperties
    { pageWidth    :: Inches
    , pageHeight   :: Inches
    , leftMargin   :: Inches
    , rightMargin  :: Inches
    , topMargin    :: Inches
    , bottomMargin :: Inches }
    deriving (Eq)

instance A.ToJSON PageProperties where
  toJSON pp = A.object
      [ "pageWidth"    .= A.toJSON (pageWidth pp)
      , "pageHeight"   .= A.toJSON (pageHeight pp)
      , "leftMargin"   .= A.toJSON (leftMargin pp)
      , "rightMargin"  .= A.toJSON (rightMargin pp)
      , "topMargin"    .= A.toJSON (topMargin pp)
      , "bottomMargin" .= A.toJSON (bottomMargin pp)]

instance A.FromJSON PageProperties where
  parseJSON (A.Object v) = do
        pw <- v .: "pageWidth"    <|> (return . Inches) 8.5
        ph <- v .: "pageHeight"   <|> (return . Inches) 11.0
        lm <- v .: "leftMargin"   <|> return quarterInch
        rm <- v .: "rightMargin"  <|> return quarterInch
        tm <- v .: "topMargin"    <|> return quarterInch
        bm <- v .: "bottomMargin" <|> return quarterInch
        return $ PageProperties pw ph lm rm tm bm
  parseJSON _ = mzero

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