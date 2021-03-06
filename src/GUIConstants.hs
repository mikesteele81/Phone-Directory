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

module GUIConstants
    ( ctrlPadding
    , lblPadding
    , winPadding
    , scale
    ) where

type DPI = Int
type RelPix = Int
type AbsPix = Int

-- |The number of pixels between controls that are grouped together.
ctrlPadding :: DPI -> AbsPix
ctrlPadding = scale 4

-- |The number of pixels between controls and their labels.
lblPadding :: DPI -> AbsPix
lblPadding = scale 3

-- |The number of pixels between the window border or vertical/horizontal
-- spacers and internal controls.
winPadding :: DPI -> AbsPix
winPadding = scale 7

scale :: RelPix -> DPI -> AbsPix
scale rel dpi = rel * dpi `div` 96
