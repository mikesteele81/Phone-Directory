module Constants where

import Graphics.PDF

units_per_inch :: Int
units_per_inch = 72
                 
pageWidth, pageHeight :: Int
pageWidth     = units_per_inch * 85 `div` 10
pageHeight    = units_per_inch * 11

col_width, col_padding :: PDFFloat
col_width = fromIntegral $ units_per_inch * 2
col_padding = fromIntegral units_per_inch / 16.0

grid_rise :: PDFFloat
grid_rise = fromIntegral units_per_inch * 9.75

font_normal :: PDFFont
font_normal    = PDFFont Helvetica 8

line_item_width, line_item_indent, line_item_leading :: PDFFloat
-- 1 7/8"
line_item_width   = col_width - 2.0 * col_padding
line_item_indent  = fromIntegral units_per_inch / 8.0
line_item_leading = fromIntegral units_per_inch / 7.0