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

prop_name_and_fsn_produce_same_json :: Name -> Property
prop_name_and_fsn_produce_same_json n = True ==> n' == fsn
  where
    n' = show . showJSON $ n
    fsn = show . showJSON . FirstSortedName $ n

prop_fsn_paul_sorts_before_pauline :: (String, String, String, String) -> Property
prop_fsn_paul_sorts_before_pauline (f, suf, l1, l2)
    = l1 /= "" && l2 /= "" && suf /= "" && f /= ""
    ==> compare paul pauline == LT
  where
    paul = FirstSortedName $ mkName f l1
    pauline = FirstSortedName $ mkName (f ++ suf) l2

prop_fsn_compare_fl_sn_ignores_ln :: (String, String) -> Property
prop_fsn_compare_fl_sn_ignores_ln (f, n)
    = f /= "" && n /= ""
    ==> (compare fl1 sn == compare fl2 sn)
  where
    fl1 = FirstSortedName $ mkName f "aaa"
    fl2 = FirstSortedName $ mkName f "zzz"
    sn  = FirstSortedName $ SingleName n

prop_name_compare_fl_sn_ignores_fn :: (String, String) -> Property
prop_name_compare_fl_sn_ignores_fn (l, n)
    = l /= "" && n /= ""
    ==> (compare fl1 sn == compare fl2 sn)
  where
    fl1 = mkName "aaa" l
    fl2 = mkName "zzz" l
    sn  = SingleName n

prop_reflective_json :: (JSON x, Eq x) => x -> Property
prop_reflective_json x
    = True
    ==> (readJSON . showJSON $ x) == return x

main :: IO ()
main = do
--  putStrLn "flowCols: return # of columns requested." 
--  quickCheckWith fewTests prop_flowCols_numColumns
--  putStrLn "flowCols: return cols of equal length"
--  quickCheckWith fewTests prop_flowCols_equalCols
--  putStrLn "flowCols: Ensure that columns do not have a leading or trailing divider."
--  quickCheckWith fewTests prop_flowCols_no_leading_dividers

    putStrLn "Name: a Name and a FirstSortedName should both have \
        \the same JSON representation."
    quickCheck prop_name_and_fsn_produce_same_json
    putStrLn "Name: Names should have reflective JSON instances."
    quickCheck (prop_reflective_json :: Name -> Property)
    putStrLn "Name: ignore first name when sorting a firstlast with a singlename"
    quickCheck prop_name_compare_fl_sn_ignores_fn
    putStrLn "FSN: ignore last name when sorting a firstlast with a singlename"
    quickCheck prop_fsn_compare_fl_sn_ignores_ln
    putStrLn "FirstSortedName: Paul should sort before Pauline"
    quickCheck prop_fsn_paul_sorts_before_pauline

    putStrLn "Priority: should have reflective JSON instances."
    quickCheck (prop_reflective_json :: Priority -> Property)

    putStrLn "ContactInfo: should have reflective JSON instances."
    quickCheck (prop_reflective_json :: ContactInfo Name -> Property)
--  where
--    fewTests = stdArgs { maxSuccess = 50 }

instance (Arbitrary a) => Arbitrary (ContactInfo a) where
    arbitrary = do
        name <- arbitrary
        priority <- arbitrary
        phone <- arbitrary
        return $ ContactInfo priority name phone
    shrink = shrinkNothing

instance Arbitrary LineItem where
    arbitrary = do
        indent <- arbitrary
        left   <- arbitrary
        right  <- arbitrary
        oneof
            [ return $ mkLabelValue indent left right
            , return Divider
            , return Blank ]
    shrink = shrinkNothing

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
    shrink = shrinkNothing

instance Arbitrary Priority where
    -- magic numbers save lots of typing.
    arbitrary = mkPriority `fmap` choose (0, 5)
    shrink = shrinkNothing
