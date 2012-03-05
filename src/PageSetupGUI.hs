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

{-# LANGUAGE OverloadedStrings #-}

module PageSetupGUI where

import Control.Applicative
import Control.Monad (liftM)
import Control.Monad.IO.Class
import Control.Monad.Trans.Error
import Graphics.UI.WX as WX
import Graphics.UI.WXCore
import Safe

import GUIConstants
import PageProperties
import UnitConversion

pageSetupDialog :: Window a -> PageProperties -> IO (Maybe PageProperties)
pageSetupDialog p prop = do
    f <- dialog p []

    -- Use to determine scaling based on a standard 96dpi view.
    Size xDpi yDpi <- bracket (clientDCCreate f) clientDCDelete
                      dcGetPPI

    -- stuff going into the main frame
    orientation <- radioBox f Vertical ["Portrait", "Landscape"]
        [ WX.text := "Orientation"
        , selection := if pageHeight prop > pageWidth prop then 0 else 1]
    pMargin      <- panel f []
    btnOK        <- button f [WX.text := "OK"]
    btnCancel    <- button f [WX.text := "Cancel"]

    -- stuff going into the margins group
    marginL <- entry pMargin [WX.text := show . unInches . leftMargin   $ prop]
    marginR <- entry pMargin [WX.text := show . unInches . rightMargin  $ prop]
    marginT <- entry pMargin [WX.text := show . unInches . topMargin    $ prop]
    marginB <- entry pMargin [WX.text := show . unInches . bottomMargin $ prop]
    set pMargin
        [ layout := boxed "Margins (inches)" $ grid 2 4
            [ [label "Left:"  , hfill $ widget marginL]
            , [label "Right:" , hfill $ widget marginR]
            , [label "Top:"   , hfill $ widget marginT]
            , [label "Bottom:", hfill $ widget marginB]
            ]
        ]

    set f
        [ picture := "data/images/pdirectory.ico"
        , WX.text := "Page Setup"
        , layout  := WX.fill . margin (winPadding xDpi) . column (ctrlPadding yDpi) $
            [ stretch $ row (ctrlPadding xDpi) . map fill $
                [ minsize (Size (scale 99 xDpi) (scale 0 yDpi)) (widget orientation)
                , widget pMargin]
            , row (ctrlPadding xDpi) [hglue, widget btnOK, widget btnCancel]
            ]
        ]

    let
        parse :: ErrorT String IO PageProperties
        parse = do
            l <- liftM (liftM Inches . readMay) $ liftIO $ get marginL WX.text
            r <- liftM (liftM Inches . readMay) $ liftIO $ get marginR WX.text
            t <- liftM (liftM Inches . readMay) $ liftIO $ get marginT WX.text
            b <- liftM (liftM Inches . readMay) $ liftIO $ get marginB WX.text
            (w, h) <- liftIO $ liftM orient2dim $ get orientation WX.selection
            maybe myError return (PageProperties w h <$> l <*> r <*> t <*> b)
          where
            myError = throwError "I'm not able to parse one or more of the margin values."

        trapError :: ErrorT String IO a -> IO ()
        trapError x = do
            e <- runErrorT x
            either (errorDialog f "error") (const $ return ()) e

    showModal f (\final -> do
        set btnOK [ on command := trapError $ do
            prop' <- parse
            liftIO $ final (Just prop')]
        set btnCancel [on command := final Nothing])

orient2dim :: Int -> (Inches, Inches)
orient2dim 0 = (Inches 8.5, Inches 11.0)
orient2dim _ = (Inches 11.0, Inches 8.5)
