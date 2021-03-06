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

module TestLineItem
  where

import Control.Monad
import Data.Char (chr)
import Data.Function (on)
import Data.List (nub)
import Graphics.PDF
import Test.QuickCheck
import Text.Regex.Posix

import IntLineItem

import TestJSON
import TestPriority

prop_flowCols_numColumns :: [LineItem] -> Positive Int -> Bool
prop_flowCols_numColumns cx (Positive n)
  = (length . flowCols cx) n == n

prop_flowCols_equalCols :: [LineItem] -> Positive Int -> Property
prop_flowCols_equalCols cx (Positive n)
  = label "flowCols always returns columns of equal length."
  $ (length . nub . map (length . unColumn) . flowCols cx) n == 1

prop_flowCols_no_leading_dividers :: NonEmptyList LineItem -> Positive Int -> Property
prop_flowCols_no_leading_dividers (NonEmpty cx) (Positive n)
    = label ""
    $ all (/= Divider) . map (head . unColumn) . flowCols cx $ n

main :: IO ()
main = return ()
--  putStrLn "flowCols: return # of columns requested." 
--  quickCheckWith fewTests prop_flowCols_numColumns
--  putStrLn "flowCols: return cols of equal length"
--  quickCheckWith fewTests prop_flowCols_equalCols
--  putStrLn "flowCols: Ensure that columns do not have a leading or trailing divider."
--  quickCheckWith fewTests prop_flowCols_no_leading_dividers
  where
    fewTests = stdArgs { maxSuccess = 50 }

instance Arbitrary LineItem where
    arbitrary = do
        left   <- arbitrary
        right  <- arbitrary
        oneof
            [ return $ mkLabelValue left right
            , return Divider
            , return Blank ]
    shrink = shrinkNothing
