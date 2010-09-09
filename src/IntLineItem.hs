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

module IntLineItem where

import Control.Monad
import Data.Function (on)
import Data.List
import Graphics.PDF hiding (leading)

import UnitConversion

font :: PDFFont
font    = PDFFont Helvetica 8

indent, leading :: PDFUnits
-- 1 7/8"
indent  = asPDFUnits . Inches $ 1 / 8
leading = asPDFUnits . Inches $ 1 / 7

col_padding :: PDFUnits
col_padding = asPDFUnits . Inches $ 1 / 16

-- | A single line making up part of a column.
data LineItem
  -- | Contact or column heading.
  = LineItem { -- |Left-justified text.
               left   :: PDFString
               -- |Right-justified text.
             , right  :: PDFString
             }
  | Header
      { left :: PDFString
      , right :: PDFString
      }
  -- | Horizontal line
  | Divider
  -- | Empty area.  This is used at the end to force columns to be of
  -- equal length.
  | Indent
  | Blank
  deriving (Eq, Show)

-- |Each page contains 4 columns of equal length.
newtype Column = Column {unColumn :: [LineItem]}
    deriving (Show)

-- |Use this to conveniently create LineItems without having to import
-- Graphics.PDF.
mkLabelValue :: String   -- ^Left
             -> String   -- ^Right
             -> LineItem
mkLabelValue l r = LineItem (toPDFString l) (toPDFString r)

mkHeader :: String -> String -> LineItem
mkHeader = Header `on` toPDFString

-- |Draw a LineItem at the given point.  The Reader monad supplies the width
-- of the line item.  Return the suggested point to draw another LineItem,
-- which is directly below this one.
drawLineItem :: (PDFUnits, PDFUnits, Point) -> LineItem
  -> Draw (PDFUnits, PDFUnits, Point)
drawLineItem (colWidth, widthRemaining, x :+ y) (LineItem l r) = do
    when (wRight > 0.0) $
        dashPattern dashStart (y' + dashHeight) dashWidth dashOffset black
    drawText $ do
        textStart (x + wPadding) y'
        setFont font
        displayText l
        textStart (wLeft + wCenter) 0
        displayText r
    return (colWidth, colWidth, x' :+ y')
  where
    dashHeight = getHeight font / 4.0
    dashWidth = max 0 $ dashEnd - dashStart
    dashStart = x + wPadding + wLeft + 5.0
    dashOffset = dashStart - x
    dashEnd = x + unPDFUnits widthRemaining - wPadding - wRight - 5.0
    wPadding = unPDFUnits col_padding
    wLeft = textWidth font l
    wCenter = unPDFUnits widthRemaining - wLeft - wRight - 2 * wPadding
    wRight = textWidth font r
    x' = x - unPDFUnits (colWidth - widthRemaining)
    y' = y - unPDFUnits leading
drawLineItem  (colWidth, widthRemaining, x :+ y) (Header l r) = do
    drawText $ do
        textStart (x + colPadding) y'
        setFont font
        textStart xOffsetL 0
        displayText l
        textStart xOffsetR 0
        displayText r
        textStart (textWidth font r - lineItemWidth) 0
    return (colWidth, colWidth, x' :+ y')
  where
    lineItemWidth = unPDFUnits widthRemaining - 2 * colPadding
    colPadding = unPDFUnits col_padding
    xOffsetL = unPDFUnits indent
    xOffsetR = lineItemWidth - textWidth font r - xOffsetL
    x' = x - unPDFUnits (colWidth - widthRemaining)
    y' = y - unPDFUnits leading

drawLineItem (colWidth, widthRemaining, x :+ y) Divider = do
    stroke (Line x y' (x + unPDFUnits colWidth) y')
    return (colWidth, colWidth, x' :+ y')
  where
    x' = x - unPDFUnits (colWidth - widthRemaining)
    y' = y - unPDFUnits leading
drawLineItem (colWidth, widthRemaining, x :+ y) Indent =
    return (colWidth, widthRemaining - indent, (x + unPDFUnits indent) :+ y)
drawLineItem (w, widthRemaining, x :+ y) Blank =
    return (w, w, x' :+ y')
  where
    x' = x - unPDFUnits (w - widthRemaining)
    y' = y - unPDFUnits leading

drawColumn :: PDFUnits -> Point -> Column -> Draw Point
drawColumn colWidth p@(x :+ y) (Column lx) = do
    (_, _, _ :+ y') <- foldM drawLineItem (colWidth, colWidth, p)
        $ columnHeading ++ lx ++ [Blank]
    stroke $ Rectangle p (x' :+ y')
    return (x' :+ y)
  where
    x' = x + unPDFUnits colWidth

truncateToMultipleOf :: PDFFloat -> PDFFloat -> PDFFloat
truncateToMultipleOf multiple number = multiple * times
  where
    times :: PDFFloat
    times = fromIntegral (truncate (number / multiple) :: Int)

dashPattern :: PDFFloat -> PDFFloat -> PDFFloat -> PDFFloat -> Color -> Draw ()
dashPattern x y w off c = do
    setDash $ DashPattern [2.0, 4.0] off
    setLineCap RoundCap
    strokeColor (Rgb 0.3 0.3 0.3)
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
    len = let (d, m) = numLines lx `divMod` n in d + if m /= 0 then 1 else 0

padColumns :: Int -> [Column] -> [Column]
padColumns n cx =
    cx ++ replicate (max 0 $ n - length cx) (Column [])

padColumn :: Int -> Column -> Column
padColumn n (Column lx) =
    Column $ lx ++ replicate (max 0 $ n - numLines lx) Blank

flow :: Int -> ([Column], [LineItem]) -> LineItem -> ([Column], [LineItem])
-- no leading dividers
flow _ r@(_, []) Divider = r
flow _ r@(_, []) Blank   = r 
flow len (cx, lx) l
    | len > numLines lx    = (cx, l:lx)
    | otherwise          = case l of
        -- no trailing dividers
        Divider -> ((Column $ reverse lx) : cx, [])
        Blank   -> ((Column $ reverse lx) : cx, [])
        _       -> flow len ((Column $ reverse lx) : cx, []) l

numLines :: [LineItem] -> Int
numLines lx = foldl' (+) 0 $ map score lx
  where
    score li = case li of
      Indent -> 0
      _      -> 1