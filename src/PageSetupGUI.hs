module PageSetupGUI where

import Graphics.UI.WX as WX

mkPageSetupWindow :: Window a -> IO (Frame ())
mkPageSetupWindow p = frameTool [] p