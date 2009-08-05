module LineItem where

import Graphics.PDF

import Constants

data LineItem = LineItem { left   :: PDFString
                         , right  :: PDFString
                         , indent :: Bool
                         } deriving (Show)
type Group = [LineItem]
newtype Column = Column { unColumn :: [Group] }

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
drawColumn (Column rx) =
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
colHeight = foldr (\g -> (+ (1 + length g))) 4 . unColumn

-- | Works just like splitAt, but peers into Columns
splitColAt :: Column -> Int -> (Column, Column)
splitColAt c n =
  let
    go :: Int -> (Column, Column) -> (Column, Column)
    go 0 g = g
    go _ x@(_, Column []) = x
    go n' (Column lx, Column (r:rx)) =
      case n' >= length r of
        True -> go (n' - length r) (Column $ lx ++ [r], Column rx)
        False -> go 0 ( Column $ lx ++ [take n' r]
                      , Column $ (drop n' r):rx)
  in
    go n (Column [], c)

-- | Flow a single column into multiple columns of equal height.  This
-- certainly has bugs in it.
flowCols :: Column -> Int -> [Column]
flowCols c n =
  let
    len = colHeight c `div` n
  in
    map fst $ take n $ tail $ iterate (\(_, c') -> splitColAt c' len) (Column [], c)
