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
import Data.List (nub)
import Graphics.PDF
import Test.QuickCheck

import ContactInfo
import LineItem
import Name
import Priority

prop_flowCols_numColumns :: [LineItem] -> Int -> Property
prop_flowCols_numColumns cx n
  = label "flowCols: return # of columns requested."
  $ n > 0
  ==> (length . flowCols cx) n == n

prop_flowCols_equalCols :: [LineItem] -> Int -> Property
prop_flowCols_equalCols cx n
  = label "flowCols: return cols of equal length"
  $ n > 0
  ==> (length . nub . map (length . unColumn) . flowCols cx) n == 1

main :: IO ()
main = do
  quickCheck prop_flowCols_numColumns
  quickCheck prop_flowCols_equalCols

instance Arbitrary Char where
  arbitrary = chr `fmap` choose (32,126)
  coarbitrary = undefined

instance (Arbitrary a) => Arbitrary (ContactInfo a) where
    arbitrary = do
        name <- arbitrary
        priority <- arbitrary
        phone <- arbitrary
        return $ ContactInfo priority name phone
    coarbitrary = undefined

instance Arbitrary LineItem where
    arbitrary = do
        indent <- arbitrary
        left   <- arbitrary
        right  <- arbitrary
        oneof
            [ return $ mkLabelValue indent left right
            , return Divider
            , return Blank ]
    coarbitrary = undefined


instance Arbitrary Name where
    arbitrary = oneof [firstLast, single]
      where
        firstLast = do
            nFirst <- arbitrary
            nLast <- arbitrary
            return $ FirstLast nFirst nLast
        single = do
            nSingle <- arbitrary
            return $ SingleName nSingle
    coarbitrary = undefined

instance Arbitrary Priority where
    -- magic numbers save lots of typing.
    arbitrary = mkPriority `fmap` choose (0, 5)
    coarbitrary = undefined
