module PDF where

import Graphics.PDF

import Constants
import Document

generate :: (Show a) => Document a -> FilePath -> IO ()
generate doc file = do
  runPdf file standardDocInfo (PDFRect 0 0 page_width page_height) $
         do renderDoc doc
