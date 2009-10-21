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

module TestContactInfo where

import Test.QuickCheck
import Text.JSON

import ContactInfo
import Name (Name)

import TestJSON
import TestName ()
import TestLineItem ()

main :: IO ()
main = do
    putStrLn "ContactInfo: Reflective JSON instance."
    quickCheck (prop_reflective_json_instance :: ContactInfo Name -> Bool)

instance (Arbitrary a) => Arbitrary (ContactInfo a) where
    arbitrary = do
        name <- arbitrary
        priority <- arbitrary
        phone <- arbitrary
        return $ ContactInfo priority name phone
    shrink = shrinkNothing

