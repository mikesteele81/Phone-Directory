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

import Control.Monad.IO.Class
import Control.Monad.Trans.Error
import Graphics.PDF

import Document
import Name
import PageProperties
import UnitConversion

-- |Create a 2-page .pdf file.  The first page sorts by last name and
-- the second sorts by first name.
generate
  :: FilePath -- ^Filename to save to.
  -> Document -- ^Document to print.  It will be automatically resorted.
  -> ErrorT String IO ()
generate file doc =
    liftIO $ runPdf file standardDocInfo
    (PDFRect 0 0 (floor . unPDFUnits . asPDFUnits . pageWidth  . pageProperties $ doc)
                 (floor . unPDFUnits . asPDFUnits . pageHeight . pageProperties $ doc))
    $ do
        renderDoc lastFirst page1 "(Sorted by Location and then Last Name)"
        renderDoc firstLast page2 "(Sorted by Location and then First Name)"
  where
    page1 = sortDoc lastFirst doc
    page2 = sortDoc firstLast doc
