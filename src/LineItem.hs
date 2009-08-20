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

import PDF

font_normal :: PDFFont
font_normal    = PDFFont Helvetica 8

line_item_width, line_item_indent, line_item_leading :: PDFFloat
-- 1 7/8"
line_item_width   = col_width - 2.0 * col_padding
line_item_indent  = fromIntegral units_per_inch / 8.0
line_item_leading = fromIntegral units_per_inch / 7.0

col_width, col_padding :: PDFFloat
col_width = fromIntegral $ units_per_inch * 2
col_padding = fromIntegral units_per_inch / 16.0

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
  deriving (Show)

-- |Each page contains 4 columns of equal length.              
newtype Column = Column {unColumn :: [LineItem]}

-- |Things which can be converted to lists of LineItems.  This
-- includes ContactInfo and Organization.
class ShowLineItems a where
    showLineItems :: a -> [LineItem]

-- |Use this to conveniently create LineItems without having to import
-- Graphics.PDF.
mkLabelValue :: Bool     -- ^Indent?
             -> String   -- ^Left
             -> String   -- ^Right
             -> LineItem
mkLabelValue i l r = LineItem (toPDFString l) (toPDFString r) i

instance Drawable LineItem where
    draw (LineItem l r i) =  
      let offset = if i then line_item_indent else 0.0
      in do
        (x :+ y) <- ask
        lift $ drawText $ do
            textStart (x + col_padding) (y - line_item_leading)
            setFont font_normal
            textStart offset 0
            displayText l
            textStart (line_item_width - textWidth font_normal r - offset) 0
            displayText r
            textStart (textWidth font_normal r - line_item_width) 0
        return (x :+ (y - line_item_leading))

    -- Horizontal line along the bottom of the drawing area.
    draw Divider = do
        (x :+ y) <- ask
        let x' = x + col_width
            y' = y - line_item_leading
        lift $ stroke (Line x y' x' y')
        return (x :+ (y - line_item_leading))

    -- Only used to fill the last column.
    draw Blank = asks (+(0 :+ (-line_item_leading)))

instance Drawable Column where
    draw (Column lx) =
      let op a = local (const a) . draw
      in do
        p@(x :+ y) <- ask
        (_ :+ y') <- foldM op p $ columnHeading ++ lx ++ [Blank]
        let x' = x + col_width
        lift $ stroke $ Rectangle p (x' :+ y')
        return (x' :+ y)

-- |This gets prepended to every Column before being drawn.
columnHeading :: [LineItem]
columnHeading = [mkLabelValue True "User Name" "Phone No.", Divider]

-- | Flow a single column into multiple columns of equal height.  This
-- certainly has bugs in it.
flowCols :: [LineItem] -- ^Column to divide up
         -> Int        -- ^Number of resultant columns
         -> [Column]   -- ^Equal length columns.  The last one may have
                       -- a few Blanks added to it.
flowCols lx n =
  let
    numBlanks = if length lx >= n then length lx `mod` n else n - (length lx `mod` n)
    -- Add a few blanks so the column will divide evenly
    c = lx ++ replicate numBlanks Blank
    len = length c `div` n
  in
    map (Column . fst) $ take n $ tail
    $ iterate (\(_, rest) -> splitAt len rest) ( [], c)
