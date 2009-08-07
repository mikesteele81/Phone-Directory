module LineItem where

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
drawLineItem _ Spacer = return ()
      
drawColumn :: Column -> PDFText ()
drawColumn rx =
  let
    op g = do
      drawLineItem line_item_width g
      startNewLine
  in do
    leading line_item_leading
    mapM_ op $ columnHeading ++ rx

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
