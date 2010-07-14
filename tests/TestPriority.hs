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

module TestPriority where

import Control.Applicative
import Test.QuickCheck

import Priority

import TestJSON

main :: IO ()
main = do
    putStrLn "ContactInfo: Reflective JSON instance."
    quickCheck (prop_reflective_json_instance :: Priority -> Bool)

instance Arbitrary Priority where
  arbitrary = mkPriority <$> arbitrary
  shrink = shrinkNothing
