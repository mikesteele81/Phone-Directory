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
import qualified PageProperties as P
import UnitConversion (unInches)

pageSetupDialog :: Window a -> P.PageProperties -> IO (Maybe P.PageProperties)
pageSetupDialog p prop = do
    f <- dialog p []

    -- stuff going into the main frame
    grpOrientation <- radioBox f Vertical ["Portrait", "Landscape"]
        [WX.text := "Orientation", selection := fromEnum . P.layout $ prop]
    pMargin      <- panel f []
    btnOK        <- button f [WX.text := "OK"]
    btnCancel    <- button f [WX.text := "Cancel"]

    -- stuff going into the margins group
    marginL <- entry pMargin [WX.text := show . unInches . P.leftMargin $ prop ]
    marginR <- entry pMargin [WX.text := show . unInches . P.rightMargin $ prop ]
    marginT <- entry pMargin [WX.text := show . unInches . P.topMargin $ prop   ]
    marginB <- entry pMargin [WX.text := show . unInches . P.bottomMargin $ prop ]
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


    result <- showModal f (\stop -> set btnOK [on command := stop (Just P.mkPageProperties)])
    return result
