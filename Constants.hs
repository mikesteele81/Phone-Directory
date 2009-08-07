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

col_width, col_padding :: PDFFloat
col_width = fromIntegral $ units_per_inch * 2
col_padding = fromIntegral units_per_inch / 16.0

grid_rise :: PDFFloat
grid_rise = fromIntegral units_per_inch * 9.75

date_inset, date_rise :: PDFFloat                 
date_inset     = page_margin
date_rise      = fromIntegral units_per_inch * 10.0

mode_inset, mode_rise :: PDFFloat
mode_inset     = title_inset
mode_rise      = date_rise

font_title, font_normal :: PDFFont
font_title     = PDFFont Helvetica 20
font_normal    = PDFFont Helvetica 8

line_item_width, line_item_indent, line_item_leading :: PDFFloat
-- 1 7/8"
line_item_width   = col_width - 2.0 * col_padding
line_item_indent  = fromIntegral units_per_inch / 8.0
line_item_leading = fromIntegral units_per_inch / 7.0