module LineItem where

import Control.Monad.Reader
import Data.List
import Graphics.PDF

import Constants

data LineItem = LineItem { left   :: PDFString
                         , right  :: PDFString
                         , indent :: Bool
                         }
              | Spacer
              deriving (Show)
              
type Column = [LineItem]

class ShowLineItems a where
    showLineItems :: a -> [LineItem]

mkLabelValue :: Bool -> String -> String -> LineItem
mkLabelValue i l r = LineItem (toPDFString l) (toPDFString r) i

drawLineItem :: LineItem -> ReaderT Point Draw ()
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
drawLineItem Spacer = do
  (x :+ y) <- ask
  lift $ stroke (Line x y (x + col_width) y)

drawColumn :: Column -> ReaderT Point Draw ()
drawColumn lx =
  let
    op d = local (+d) . drawLineItem
    ds = iterate (+ (0 :+ (-line_item_leading))) (0 :+ 0)
    br = ( col_width
           :+ (-1 * (fromIntegral . (+3) . length) lx * line_item_leading))
  in do
    p <- ask
    lift $ stroke (Rectangle p (p + br))
    zipWithM_ op ds $ columnHeading ++ lx

columnHeading :: Column
columnHeading = [mkLabelValue True "User Name" "Phone No.", Spacer]

-- | Flow a single column into multiple columns of equal height.  This
-- certainly has bugs in it.
flowCols :: Column -> Int -> [Column]
flowCols c n =
  let
    len = length c `div` n
  in
    map fst $ take n $ tail $ iterate (\(_, c') -> splitAt len c') ( [], c)
