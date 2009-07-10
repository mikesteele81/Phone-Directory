module Main where

import Graphics.PDF

units_per_inch = 72
page_width     = units_per_inch * 85 `div` 10
page_height    = units_per_inch * 11
page_margin    = units_per_inch `div` 4

title_inset    = units_per_inch * 175 `div` 100

font_title     = PDFFont Helvetica 20

main = runPdf "test.pdf" standardDocInfo (PDFRect 0 0 page_width page_height) pdf
    where
      pdf = do
        p <- addPage Nothing
        drawWithPage p $ drawText $
                     text font_title
                              (fromIntegral title_inset )
                              (fromIntegral (page_height - page_margin) - getHeight font_title)
                              (toPDFString "PHONE DIRECTORY")
