module LineItem where

import Data.List
import Graphics.PDF

import Constants

data LineItem = LineItem { left   :: PDFString
                         , right  :: PDFString
                         , indent :: Bool
                         } deriving (Show)
type Group = [LineItem]
type Column = [Group]

class ShowLineItems a where
    showLineItems :: a -> [LineItem]

mkLabelValue :: Bool -> String -> String -> LineItem
mkLabelValue i l r = LineItem (toPDFString l) (toPDFString r) i
                     
-- | Draw a LineItem
drawLineItem :: PDFFloat -> LineItem -> PDFText ()
drawLineItem w (LineItem l r i) =
    let offset = if i then line_item_indent else 0.0
    in do
      setFont font_normal
      textStart offset 0
      displayText l
      textStart (w - textWidth font_normal r - offset) 0
      displayText r
      textStart (textWidth font_normal r - w) 0
      
drawLineItems :: PDFFloat -> [LineItem] -> PDFText ()
drawLineItems w lx =
    let op l = drawLineItem w l >> startNewLine
    in do
      leading line_item_leading
      -- draw each contact
      mapM_ op lx

drawColumn :: Column -> PDFText ()
drawColumn rx =
  let
    op g = do
      leading org_leading
      drawLineItems line_item_width g
      startNewLine
  in
    mapM_ op $ columnHeading : rx

columnHeading :: Group
columnHeading = [mkLabelValue True "User Name" "Phone No."]

-- | The height of all columns plus the column header and bottom line  in
-- units of LineItems.
colHeight :: Column -> Int
colHeight gx = (sum . map length) gx + length gx - 1

-- | Works just like splitAt, but peers into Columns
splitColAt :: Column -> Int -> (Column, Column)
splitColAt c n =
  let
    go :: Int -> (Column, Column) -> (Column, Column)
    go 0 x = x
    go _ x@(_, []) = x
    go n' (lx, r:rx) =
      if n' >= 1 + length r then
        go (n' - length r - 1) (lx ++ [r], rx)
      else
        go 0 ( lx ++ [take n' r], drop n' r : rx)
  in
    go n ( [], c)

-- | Flow a single column into multiple columns of equal height.  This
-- certainly has bugs in it.
flowCols :: Column -> Int -> [Column]
flowCols c n =
  let
    len = colHeight c `div` n
  in
    map fst $ take n $ tail $ iterate (\(_, c') -> splitColAt c' len) ( [], c)
