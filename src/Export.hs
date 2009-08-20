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


module Export
    ( generate
    ) where

import Graphics.PDF

import ContactInfo
import Document
import Name
import PDF

pageWidth, pageHeight :: Int
pageWidth     = units_per_inch * 85 `div` 10
pageHeight    = units_per_inch * 11

-- |Create a 2-page .pdf file.  The first page sorts by last name and
-- the second sorts by first name.
generate
  :: Document (ContactInfo Name) -- ^Document to print.  It will be
                                 -- automatically resorted.
  -> FilePath                    -- ^Filename to save to.
  -> IO ()
generate doc file =
  let
    page1 = sortDoc doc
    page2 = sortDoc $ fmap (fmap FirstSortedName) doc
  in
    runPdf file standardDocInfo (PDFRect 0 0 pageWidth pageHeight) $ do
      renderDoc page1 "(Sorted by Location and then Last Name)"
      renderDoc page2 "(Sorted by Location and then First Name)"
