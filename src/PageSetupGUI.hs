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

import Control.Monad (liftM)
import Graphics.UI.WX as WX

import GUIConstants
import PageProperties as P
import UnitConversion

pageSetupDialog :: Window a -> P.PageProperties -> IO (Maybe P.PageProperties)
pageSetupDialog p prop = do
    f <- dialog p []

    -- stuff going into the main frame
    grpOrientation <- radioBox f Vertical ["Portrait", "Landscape"]
        [WX.text := "Orientation", selection :=
            if pageHeight prop > pageWidth prop then 0 else 1]
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

    let
        parse :: IO P.PageProperties
        parse = do
            l <- liftM (Inches . read) $ get marginL WX.text
            r <- liftM (Inches . read) $ get marginR WX.text
            t <- liftM (Inches . read) $ get marginT WX.text
            b <- liftM (Inches . read) $ get marginB WX.text
            (w, h) <- liftM orient2dim $ get grpOrientation WX.selection
            return $ P.PageProperties w h l r t b

    showModal f (\final -> do
        set btnOK [ on command := do
            prop' <- parse
            final (Just prop')]
        set btnCancel [on command := final Nothing])

orient2dim :: Int -> (Inches, Inches)
orient2dim 0 = (Inches 8.5, Inches 11.0)
orient2dim _ = (Inches 11.0, Inches 8.5)