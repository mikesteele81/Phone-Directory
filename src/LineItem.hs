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

import Constants

data LineItem = LineItem { left   :: PDFString
                         , right  :: PDFString
                         , indent :: Bool
                         }
              | Divider
              | Blank
              deriving (Show)
              
type Column = [LineItem]

class ShowLineItems a where
    showLineItems :: a -> [LineItem]

mkLabelValue :: Bool -> String -> String -> LineItem
mkLabelValue i l r = LineItem (toPDFString l) (toPDFString r) i

drawLineItem :: LineItem -> ReaderT Point Draw (Point)
drawLineItem (LineItem l r i) =  
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
drawLineItem Divider = do
  (x :+ y) <- ask
  let x' = x + col_width
      y' = y - line_item_leading
  lift $ stroke (Line x y' x' y')
  return (x :+ (y - line_item_leading))
drawLineItem Blank = asks (+(0 :+ (-line_item_leading)))

drawColumn :: Column -> ReaderT Point Draw (Point)
drawColumn lx =
  let op a = local (const a) . drawLineItem
  in do
    p@(x :+ y) <- ask
    (_ :+ y') <- foldM op p $ columnHeading ++ lx ++ [Blank]
    let x' = x + col_width
    lift $ stroke $ Rectangle p (x' :+ y')
    return (x' :+ y)

columnHeading :: Column
columnHeading = [mkLabelValue True "User Name" "Phone No.", Divider]

-- | Flow a single column into multiple columns of equal height.  This
-- certainly has bugs in it.
flowCols :: Column -> Int -> [Column]
flowCols c n =
  let
    -- Add a few blanks so the column will divide evenly
    c' = c ++ replicate (length c `mod` n) Blank
    len = length c' `div` n
  in
    map fst $ take n $ tail $ iterate (\(_, rest) -> splitAt len rest) ( [], c')
