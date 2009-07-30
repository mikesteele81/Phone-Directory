module GUI where

import Control.Applicative
import Graphics.UI.WX
import Graphics.UI.WXCore hiding ( Document )

import Document
import Name
import Organization

file_types_selection :: [(String, [String])]
file_types_selection = [("JSON", ["*.json"])]

-- | Create a Document object based on the tree.      
tree2Doc :: TreeCtrl a -> IO (Maybe (Document Name))
tree2Doc tc = do
  root <- treeCtrlGetRootItem tc
  orgs <- treeCtrlWithChildren tc root $ \itm -> do
    orgCI <- unsafeTreeCtrlGetItemClientData tc itm
    contacts <- treeCtrlWithChildren tc itm $ \itm' -> do
      unsafeTreeCtrlGetItemClientData tc itm'
    return $ Organization <$> orgCI <*> sequence contacts
  return $ Document "TODO - add date" <$> sequence orgs
