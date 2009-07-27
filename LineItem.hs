module LineItem where

import Graphics.PDF

import Constants

data LineItem = LabelValue { left   :: PDFString
                           , right  :: PDFString
                           , indent :: Bool }
              | DividingLine
              deriving (Show)
              
class ShowLineItems a where
    showLineItems :: a -> [LineItem]

mkLabelValue :: (Show a, Show b) => Bool -> a -> b -> LineItem
mkLabelValue i l r = LabelValue (toPDFString $ show l)
                     (toPDFString $ show r) i
                     
-- | Draw a LineItem
drawLineItem :: PDFFloat -> LineItem -> PDFText ()
drawLineItem w (LabelValue l r i) =
    let offset = if i then line_item_indent else 0.0
    in do
      setFont font_normal
      textStart offset 0
      displayText l
      textStart (w - textWidth font_normal r - offset) 0
      displayText r
      textStart (textWidth font_normal r - w) 0
      
drawLineItem _ DividingLine = return ()

newtype Column = Column [LineItem]
                
columnHeading :: LineItem
columnHeading = LabelValue
                { left   = toPDFString "User Name"
                , right  = toPDFString "Phone No."
                , indent = True }

-- | The height of all columns plus the column header and bottom line 
colHeight :: Column -> PDFFloat
colHeight (Column lx) = line_item_leading
                        * fromIntegral (4 + length lx)

-- | Flow a single column into multiple columns of equal length.
flowCols :: [LineItem] -> Int -> [Column]
flowCols lx n = go lx
    where
      go :: [LineItem] -> [Column]
      go [] = []
      go c' = Column (take n c') : go (drop n c')
