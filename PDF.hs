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
    runPdf file standardDocInfo (PDFRect 0 0 page_width page_height) $ do
      renderDoc page1 "(Sorted by Location and then Last Name)"
      renderDoc page2 "(Sorted by Location and then First Name)"
