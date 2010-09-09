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
import Graphics.PDF hiding (leading)

import UnitConversion

font :: PDFFont
font = PDFFont Helvetica 8
dashHeight :: PDFFloat
dashHeight  = getHeight font / 4.0

indent, leading :: PDFUnits
-- 1 7/8"
indent  = asPDFUnits . Inches $ 1 / 8
leading = asPDFUnits . Inches $ 1 / 7

col_padding :: PDFUnits
col_padding = asPDFUnits . Inches $ 1 / 16

-- | A single line making up part of a column.
data LineItem
  -- | Contact or column heading. LineItem left right isDashed
  = LineItem PDFString PDFString Bool
  -- | Horizontal line
  | Divider
  -- | Empty area.  This is used at the end to force columns to be of
  -- equal length.
  | Indent
  | Blank
  deriving (Eq, Show)

-- |Each page contains 4 columns of equal length.
newtype Column = Column { unColumn :: [LineItem]} deriving (Show)

-- |Use this to conveniently create LineItems without having to import
-- Graphics.PDF. Dashes are set if the right-hand string exists.
mkLabelValue :: String   -- ^Left
             -> String   -- ^Right
             -> LineItem
mkLabelValue l r = LineItem (toPDFString l) (toPDFString r) (not . null $ r)

-- |mkLabelValue sets dashing enabled by default.  Use this to change it.
setDashed :: Bool -> LineItem -> LineItem
setDashed d (LineItem l r _) = LineItem l r d
setDashed _ li = li

mkHeader :: String -> String -> LineItem
mkHeader l r = setDashed False $ mkLabelValue l r

-- |Draw a LineItem at the given point.  The Reader monad supplies the width
-- of the line item.  Return the suggested point to draw another LineItem,
-- which is directly below this one.
drawLineItem :: (PDFUnits, PDFUnits, Point) -> LineItem
  -> Draw (PDFUnits, PDFUnits, Point)
drawLineItem (colWidth, widthRemaining, x :+ y) Indent =
    return (colWidth, widthRemaining - indent, (x + unPDFUnits indent) :+ y)
drawLineItem (colWidth, widthRemaining, x :+ y) li = do
    case li of
      LineItem l r d -> do
          when d $ dashPattern dStart (y' + dashHeight) dWidth dOffset black
          drawText $ do
              textStart (x + wPadding) y'
              setFont font
              displayText l
              textStart (wLeft + wCenter) 0
              displayText r
        where
          dEnd    = x + unPDFUnits widthRemaining - wPadding - wRight - 5.0
          dWidth  = max 0 $ dEnd - dStart
          dOffset = dStart - x
          dStart  = x + wPadding + wLeft + 5.0
          wCenter = unPDFUnits widthRemaining - wLeft - wRight - 2 * wPadding
          wRight  = textWidth font r
          wLeft   = textWidth font l
      Divider -> stroke (Line x y' (x + unPDFUnits colWidth) y')
      Blank -> return ()
      Indent -> error "Can't get here"
    return (colWidth, colWidth, x' :+ y')
  where
    wPadding = unPDFUnits col_padding
    x'       = x - unPDFUnits (colWidth - widthRemaining)
    y'       = y - unPDFUnits leading

drawColumn :: PDFUnits -> Point -> Column -> Draw Point
drawColumn colWidth p@(x :+ y) (Column lx) = do
    (_, _, _ :+ y') <- foldM drawLineItem (colWidth, colWidth, p)
        $ columnHeading ++ lx ++ [Blank]
    stroke $ Rectangle p (x' :+ y')
    return (x' :+ y)
  where
    x' = x + unPDFUnits colWidth

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
columnHeading = [Indent, mkHeader "Name" "Phone Number", Divider]

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
    len = let (d, m) = numLines (Column lx) `divMod` n in d + if m /= 0 then 1 else 0

padColumns :: Int -> [Column] -> [Column]
padColumns n cx =
    cx ++ replicate (max 0 $ n - length cx) (Column [])

padColumn :: Int -> Column -> Column
padColumn n c = Column $ unColumn c ++ replicate (max 0 $ n - numLines c) Blank

flow :: Int -> ([Column], [LineItem]) -> LineItem -> ([Column], [LineItem])
-- no leading dividers
flow _ r@(_, []) Divider = r
flow _ r@(_, []) Blank   = r 
flow len (cx, lx) l
    | len > numLines (Column lx) = (cx, l:lx)
    | otherwise          = case l of
        -- no trailing dividers
        Divider -> ((Column $ reverse lx) : cx, [])
        Blank   -> ((Column $ reverse lx) : cx, [])
        _       -> flow len ((Column $ reverse lx) : cx, []) l

numLines :: Column -> Int
numLines (Column lx) = length . filter (/= Indent) $ lx
