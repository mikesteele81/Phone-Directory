module Main where

import Graphics.PDF

units_per_inch = 72
                 
page_width, page_height :: Int
page_width     = units_per_inch * 85 `div` 10
page_height    = units_per_inch * 11

page_margin, title_inset, title_rise :: PDFFloat
page_margin    = fromIntegral units_per_inch / 4.0
title_inset    = fromIntegral units_per_inch * 1.75
title_rise     = fromIntegral units_per_inch * 10.5
title_string   = toPDFString "PHONE DIRECTORY"
                 
date_inset     = page_margin
date_rise      = fromIntegral units_per_inch * 10.0
date_string    = toPDFString "Revised: 07/01/09"

mode_inset     = title_inset
mode_rise      = date_rise
mode_string    = toPDFString "(Sorted by Location and then First Name)"

font_title     = PDFFont Helvetica 20
font_normal    = PDFFont Helvetica 10

main = runPdf "test.pdf" standardDocInfo (PDFRect 0 0 page_width page_height) $
       do
         p <- addPage Nothing
         drawWithPage p $
          do
            drawText $ text font_title title_inset title_rise title_string
            drawText $ text font_normal date_inset date_rise date_string
            drawText $ text font_normal mode_inset mode_rise mode_string
