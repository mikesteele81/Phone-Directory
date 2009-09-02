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

module PDF where

import Control.Monad.Cont
import Graphics.PDF

units_per_inch :: Int
units_per_inch = 72

-- |Draw a LineItem so that whatever Point is in the monad sits on the
-- upper-left corner of the bounding box of what's drawn.
class Drawable a where
    draw :: Point                 -- ^Where to draw it
        -> a                      -- ^Thing to draw
        -> ContT Point Draw Point -- ^Suggested location to draw the next
                                  -- Drawable. This will be the lowel-left
                                  -- corner of the current column.



