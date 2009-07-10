module Main where

import Graphics.PDF

units_per_inch = 72
page_width = units_per_inch * 85 `div` 10
page_height = units_per_inch * 11

main = runPdf "test.pdf" standardDocInfo (PDFRect 0 0 page_width page_height) pdf
    where
      pdf = do
        p <- addPage Nothing
        drawWithPage p $ drawText $
          sequence $ replicate 10 $
            text (PDFFont Helvetica 10) 10 10 (toPDFString "ABC")
