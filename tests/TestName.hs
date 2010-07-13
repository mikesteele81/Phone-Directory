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

module TestName where

import Control.Applicative
import Data.Convertible.Text
import Test.QuickCheck

import Name

import TestJSON

main :: IO ()
main = do
    putStrLn "Name: Reflective JSON instance."
    quickCheck (prop_reflective_json_instance :: Name -> Bool)
    putStrLn "Name: ignore first name when sorting a firstlast with a singlename"
    quickCheck prop_name_compare_fl_sn_ignores_fn
    putStrLn "FSN: ignore last name when sorting a firstlast with a singlename"
    quickCheck prop_fsn_compare_fl_sn_ignores_ln
    putStrLn "FirstSortedName: Paul should sort before Pauline"
    quickCheck prop_fsn_paul_sorts_before_pauline

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

