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


module PageSetupGUI where

import Graphics.UI.WX as WX

import GUIConstants

mkPageSetupWindow :: Window a -> IO (Frame ())
mkPageSetupWindow p = do
    f <- frameTool [] p

    -- stuff going into the main frame
    grpOrientation <- radioBox f Vertical ["Portrait", "Landscape"]
        [WX.text := "Orientation"]
    pMargin      <- panel f []
    btnOK        <- button f [WX.text := "OK"]
    btnCancel    <- button f [WX.text := "Cancel"]

    -- stuff going into the margins group
    marginL <- entry pMargin []
    marginR <- entry pMargin []
    marginT <- entry pMargin []
    marginB <- entry pMargin []
    set pMargin
        [ layout := boxed "Margins (inches)" $ grid 4 2
            [ [ label "Left:", widget marginL, label "Right:", widget marginR ]
            , [ label "Top:", widget marginT, label "Bottom:", widget marginB ] ]
        ]

    set f
        [ picture := "data/images/pdirectory.ico"
        , WX.text := "Page Setup"
        , layout  := WX.fill $ margin winPadding $ column ctrlPadding
            [ row ctrlPadding [vfill $ widget grpOrientation, widget pMargin]
            , row ctrlPadding [hglue, widget btnOK, widget btnCancel]]
        ]

    return f
