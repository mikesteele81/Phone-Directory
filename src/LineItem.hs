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

module LineItem where

import Control.Monad
import Data.Function (on)
import Data.List
import Graphics.PDF

import UnitConversion

font_normal :: PDFFont
font_normal    = PDFFont Helvetica 8

line_item_indent, line_item_leading :: PDFUnits
-- 1 7/8"
line_item_indent  = asPDFUnits . Inches $ 1 / 8
line_item_leading = asPDFUnits . Inches $ 1 / 7

col_padding :: PDFUnits
col_padding = asPDFUnits . Inches $ 1 / 16

-- | A single line making up part of a column.
data LineItem
  -- | Contact or column heading.
  = LineItem { -- |Left-justified text.
               left   :: PDFString
               -- |Right-justified text.
             , right  :: PDFString
               -- |Should 'left' be indented a little?
               -- This is so group membership can be made obvious.
             , indent :: Bool
             }
  | Header
      { left :: PDFString
      , right :: PDFString
      }
  -- | Horizontal line
  | Divider
  -- | Empty area.  This is used at the end to force columns to be of
  -- equal length.
  | Blank
  deriving (Eq, Show)

-- |Each page contains 4 columns of equal length.
newtype Column = Column {unColumn :: [LineItem]}
    deriving (Show)

-- |Things which can be converted to lists of LineItems.  This
-- includes ContactInfo and Organization.
class ShowLineItems a where
    showLineItems :: a -> [LineItem]

instance (ShowLineItems a) => ShowLineItems [a] where
    showLineItems = intercalate [Divider] . map showLineItems

-- |Use this to conveniently create LineItems without having to import
-- Graphics.PDF.
mkLabelValue :: Bool     -- ^Indent?
             -> String   -- ^Left
             -> String   -- ^Right
             -> LineItem
mkLabelValue i l r = LineItem (toPDFString l) (toPDFString r) i

mkHeader :: String -> String -> LineItem
mkHeader = (Header `on` toPDFString)

-- |Draw a LineItem at the given point.  The Reader monad supplies the width
-- of the line item.  Return the suggested point to draw another LineItem,
-- which is directly below this one.
drawLineItem :: PDFUnits -> Point -> LineItem -> Draw Point
drawLineItem colWidth (x :+ y) (LineItem  l r i) = do
    dashPattern (x + colPadding + xOffsetL + textWidth font_normal l)
       (y' + dashHeight) (xOffsetR - textWidth font_normal l) black
    drawText $ do
        textStart (x + colPadding) y'
        setFont font_normal
        textStart xOffsetL 0
        displayText l
        textStart xOffsetR 0
        displayText r
        textStart (textWidth font_normal r - lineItemWidth) 0
    return (x :+ y')
  where
    dashHeight = getHeight font_normal / 4.0
    lineItemWidth = unPDFUnits colWidth - 2 * colPadding
    colPadding = unPDFUnits col_padding
    xOffsetL = if i then unPDFUnits line_item_indent else 0.0
    xOffsetR = lineItemWidth - textWidth font_normal r - xOffsetL
    y' = y - unPDFUnits line_item_leading
drawLineItem colWidth (x :+ y) (Header  l r) = do
    drawText $ do
        textStart (x + colPadding) y'
        setFont font_normal
        textStart xOffsetL 0
        displayText l
        textStart xOffsetR 0
        displayText r
        textStart (textWidth font_normal r - lineItemWidth) 0
    return (x :+ y')
  where
    lineItemWidth = unPDFUnits colWidth - 2 * colPadding
    colPadding = unPDFUnits col_padding
    xOffsetL = unPDFUnits line_item_indent
    xOffsetR = lineItemWidth - textWidth font_normal r - xOffsetL
    y' = y - unPDFUnits line_item_leading

drawLineItem colWidth (x :+ y) Divider = do
    stroke (Line x y' (x + unPDFUnits colWidth) y')
    return $ x :+ y'
  where
    y' = y - unPDFUnits line_item_leading
drawLineItem _ (x :+ y) Blank = do
    return $ x :+ (y - unPDFUnits line_item_leading)

drawColumn :: PDFUnits -> Point -> Column -> Draw Point
drawColumn colWidth p@(x :+ y) (Column lx) = do
    (_ :+ y') <- foldM (drawLineItem colWidth) p $ columnHeading ++ lx ++ [Blank]
    stroke $ Rectangle p (x' :+ y')
    return (x' :+ y)
  where
    x' = x + unPDFUnits colWidth

dashPattern :: PDFFloat -> PDFFloat -> PDFFloat -> Color -> Draw ()
dashPattern x y w c = do
    setDash $ DashPattern [1.0, 4.0] 1.0
    strokeColor (Rgb 0.5 0.5 0.5)
    stroke $ Line x y (x + w) y
    strokeColor c
    setNoDash

-- |This gets prepended to every Column before being drawn.
columnHeading :: [LineItem]
columnHeading = [mkHeader "Name" "Phone Number", Divider]

-- | Flow a single column into multiple columns of equal height.  This
-- certainly has bugs in it.
flowCols :: [LineItem] -- ^Column to divide up
         -> Int        -- ^Number of resultant columns
         -> [Column]   -- ^Equal length columns.  The last one may have
                       -- a few Blanks added to it.
flowCols lx n =
    map (padColumn len) . padColumns n $ cx'
  where
    (cx, lx') = foldl (flow len) ([], []) lx
    cx' = reverse $ Column (reverse lx') : cx
    len = let (d, m) = length lx `divMod` n in d + if m /= 0 then 1 else 0

padColumns :: Int -> [Column] -> [Column]
padColumns n cx =
    cx ++ replicate (max 0 $ n - length cx) (Column [])

padColumn :: Int -> Column -> Column
padColumn n (Column lx) =
    Column $ lx ++ replicate (max 0 $ n - length lx) Blank

flow :: Int -> ([Column], [LineItem]) -> LineItem -> ([Column], [LineItem])
-- no leading dividers
flow _ r@(_, []) Divider = r
flow _ r@(_, []) Blank   = r 
flow len (cx, lx) l
    | len > length lx    = (cx, l:lx)
    | otherwise          = case l of
        -- no trailing dividers
        Divider -> ((Column $ reverse lx) : cx, [])
        Blank   -> ((Column $ reverse lx) : cx, [])
        _       -> flow len ((Column $ reverse lx) : cx, []) l
