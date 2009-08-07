{- This file is part of PhoneDirectory.

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

module PDF where

import Graphics.PDF

import Constants
import Document
import Name

generate :: Document Name -> FilePath -> IO ()
generate doc file =
  let
    page1 = sortDoc doc
    page2 = sortDoc $ fmap FirstSortedName doc
  in
    runPdf file standardDocInfo (PDFRect 0 0 pageWidth pageHeight) $ do
      renderDoc page1 "(Sorted by Location and then Last Name)"
      renderDoc page2 "(Sorted by Location and then First Name)"
