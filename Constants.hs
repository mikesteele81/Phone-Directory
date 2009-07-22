module Constants where

import Graphics.PDF

units_per_inch :: Int
units_per_inch = 72
                 
page_width, page_height :: Int
page_width     = units_per_inch * 85 `div` 10
page_height    = units_per_inch * 11

page_margin, title_inset, title_rise :: PDFFloat
page_margin    = fromIntegral units_per_inch / 4.0
title_inset    = fromIntegral units_per_inch * 1.75
title_rise     = fromIntegral units_per_inch * 10.5

title_string :: PDFString
title_string   = toPDFString "PHONE DIRECTORY"

date_inset, date_rise :: PDFFloat                 
date_inset     = page_margin
date_rise      = fromIntegral units_per_inch * 10.0
date_string :: PDFString
date_string    = toPDFString "Revised: 07/01/09"

mode_inset, mode_rise :: PDFFloat
mode_inset     = title_inset
mode_rise      = date_rise
mode_string :: PDFString
mode_string    = toPDFString "(Sorted by Location and then First Name)"

font_title, font_normal :: PDFFont
font_title     = PDFFont Helvetica 20
font_normal    = PDFFont Helvetica 10

