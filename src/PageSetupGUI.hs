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
