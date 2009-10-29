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

import Control.Monad.Reader
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

-- |Draw a LineItem at the given point.  The Reader monad supplies the width
-- of the line item.  Return the suggested point to draw another LineItem,
-- which is directly below this one.
drawLineItem :: Point -> LineItem -> ReaderT PDFUnits Draw Point
drawLineItem (x :+ y) (LineItem  l r i) = do
    colWidth <- asks unPDFUnits
    let lineItemWidth = colWidth - 2 * colPadding
        y' = y - unPDFUnits line_item_leading
    lift . drawText $ do
        textStart (x + colPadding) y'
        setFont font_normal
        textStart offset 0
        displayText l
        textStart (lineItemWidth - textWidth font_normal r - offset) 0
        displayText r
        textStart (textWidth font_normal r - lineItemWidth) 0
    return (x :+ y')
  where
    lineItemIndent = unPDFUnits line_item_indent
    colPadding = unPDFUnits col_padding
    offset = if i then lineItemIndent else 0.0
drawLineItem (x :+ y) Divider = do
    width <- asks unPDFUnits
    lift $ stroke (Line x y' (x + width) y')
    return $ x :+ y'
  where
    y' = y - unPDFUnits line_item_leading
drawLineItem (x :+ y) Blank = do
    return $ x :+ (y - unPDFUnits line_item_leading)

drawColumn :: PDFUnits -> Point -> Column -> Draw Point
drawColumn colWidth p@(x :+ y) (Column lx) = do
    (_ :+ y') <- runReaderT (foldM drawLineItem p $ columnHeading ++ lx ++ [Blank]) colWidth
    stroke $ Rectangle p (x' :+ y')
    return (x' :+ y)
  where
    x' = x + unPDFUnits colWidth

-- |This gets prepended to every Column before being drawn.
columnHeading :: [LineItem]
columnHeading = [mkLabelValue True "Name" "Phone Number", Divider]

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
