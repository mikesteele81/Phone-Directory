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

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}

module TestOrganization where

import Control.Applicative
import Control.Monad (liftM)
import Data.Attempt
import Test.QuickCheck

import ContactInfo
import Data.List
import Name (Name)
import Organization

import TestContactInfo ()
import TestJSON
import TestName ()

main :: IO ()
main = do
  putStrLn "Organization: Reflective JSON instance."
  quickCheck (prop_reflective_json_instance :: Organization Name -> Bool)
  putStrLn "Organization: Merging two organizations with the same info should\
           \result in a single merged organization."
  quickCheck prop_merging_like_orgs
  putStrLn "Organization: Merging two organizations with the different should \
           \result in a arguments tupled."
  quickCheck prop_merging_unlike_orgs
  putStrLn "Organization: Merging a list of Organizations with itself results \
           \in a list the same length with double the number of contacts."
  quickCheck prop_orgs_merged_with_self
  putStrLn "Organization: When merging an org with a list of orgs..."
  quickCheck prop_org_merged_with_orgs
  putStrLn "Organization: CSV conversion."
  quickCheck prop_reflective_csv_conversion

instance (Arbitrary a) => Arbitrary (Organization a) where
    arbitrary = Organization <$> arbitrary <*> arbitrary
    shrink (Organization i cx) = [Organization x cx' | x <- shrink i]
      where cx' = if null cx then [] else tail cx

prop_merging_like_orgs :: ( ContactInfo Name, [ContactInfo Name]
                          , [ContactInfo Name]) -> Bool
prop_merging_like_orgs (oi, cx, cx2) =
    case mergeOp (Just o1) o2 of
      (Nothing, Organization _ cx3) -> sort cx3 == sort (cx ++ cx2)
      --o1 and o2 have the same info, so this shouldn't happen
      (Just _, _) -> False
  where
    o1 = Organization oi cx
    o2 = Organization oi cx2

prop_merging_unlike_orgs
  :: (ContactInfo Name, ContactInfo Name
     ,[ContactInfo Name], [ContactInfo Name]) -> Property
prop_merging_unlike_orgs (oi1, oi2, cx1, cx2) =
    oi1 /= oi2 ==>
    case mergeOp (Just o1) o2 of
      --these orgs should not merge
      (Nothing, _) -> False
      (Just o1', o2') -> o1' == o1 && o2' == o2
  where
    o1 = Organization oi1 cx1
    o2 = Organization oi2 cx2

prop_orgs_merged_with_self :: [Organization Name] -> Bool
prop_orgs_merged_with_self ox
    =  length ox' == length ox''
    && 2 * length (contacts ox') == length (contacts ox'')
  where
    -- do this in case the generated orgs has duplicates.
    ox' = mergeOrgs ox
    ox'' = mergeOrgs (ox' ++ ox')
    contacts = concatMap oContacts

prop_org_merged_with_orgs :: [Organization Name] -> Bool
prop_org_merged_with_orgs [] = True
prop_org_merged_with_orgs [_] = True
prop_org_merged_with_orgs (mergeOrgs -> (o:ox)) =
    case ox' of
      [] -> False
      [o'] -> length ox == 1
              && sort (oContacts o') == sort (concatMap oContacts (o:ox))
      (o':ox'') | o' == o -> ox == ox''
                | otherwise -> length (union (o:ox) (o':ox'')) == length (o:ox) + 1
  where
    ox' = mergeOrg o ox

-- | going to CSV and back again does _not_ produce the original 
--   organization. However, going from Org to CSV to org to CSV is the same 
--   asgoing from Org to CSV.
prop_reflective_csv_conversion :: Organization (ContactInfo Name) -> Bool
prop_reflective_csv_conversion o = attempt (const False) (==o') ao
  where
    ao = liftM (sort . concatMap toCSV . mergeOrgs) . fromCSV . toCSV $ o
    o' = sort . toCSV $ o
