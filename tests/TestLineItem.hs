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
import Text.JSON
import Text.Regex.Posix

import ContactInfo
import LineItem
import Name
import Priority

prop_flowCols_numColumns :: [LineItem] -> Int -> Property
prop_flowCols_numColumns cx n
  = n > 0
  ==> (length . flowCols cx) n == n

prop_flowCols_equalCols :: [LineItem] -> Int -> Property
prop_flowCols_equalCols cx n
  = n > 0
  ==> (length . nub . map (length . unColumn) . flowCols cx) n == 1

prop_flowCols_no_leading_dividers :: [LineItem] -> Int -> Property
prop_flowCols_no_leading_dividers cx n
    = n > 0 && (not $ null cx)
    ==> all (/= Divider) . map (head . unColumn) . flowCols cx $ n

prop_name_and_fsn_produce_same_json :: Name -> Property
prop_name_and_fsn_produce_same_json n = True ==> n' == fsn
  where
    n' = show . showJSON $ n
    fsn = show . showJSON . FirstSortedName $ n

prop_reflective_json_name :: Name -> Property
prop_reflective_json_name n
    = True
    ==> (readJSON . showJSON $ n) == return n

main :: IO ()
main = do
  putStrLn "flowCols: return # of columns requested." 
  quickCheck prop_flowCols_numColumns
  putStrLn "flowCols: return cols of equal length"
  quickCheck prop_flowCols_equalCols
  putStrLn "flowCols: Ensure that columns do not have a leading or trailing divider."
  quickCheck prop_flowCols_no_leading_dividers

  putStrLn "Name: a Name and a FirstSortedName should both have \
      \the same JSON representation."
  quickCheck prop_name_and_fsn_produce_same_json
  putStrLn "Name: Names should have reflective JSON instances."
  quickCheck prop_reflective_json_name

instance Arbitrary Char where
  arbitrary = chr `fmap` oneof [choose (65, 90), choose (97, 122)]
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
