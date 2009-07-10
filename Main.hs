module Main where

import Graphics.PDF

main = runPdf "test.pdf" standardDocInfo (PDFRect 0 0 100 100) pdf
    where
      pdf = do
        p <- addPage Nothing
        drawWithPage p $ drawText $
          sequence $ replicate 10 $
            text (PDFFont Helvetica 10) 10 10 (toPDFString "ABC")
