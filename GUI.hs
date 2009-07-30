module GUI where

import Control.Applicative
import Graphics.UI.WXCore hiding ( Document )

import Document
import Name
import Organization

file_types_selection :: [(String, [String])]
file_types_selection = [("JSON", ["*.json"])]

populateTree :: TreeCtrl a -> Document Name -> IO ()
populateTree tc doc =
    let addItem p itm = do
          tc' <- treeCtrlAppendItem tc p (show itm) 0 0 objectNull
          treeCtrlSetItemClientData tc tc' (return ()) itm
          return tc'
        populateOrg p org = do
          orgTc <- addItem p $ oInfo org
          mapM_ (addItem orgTc) $ oContacts org
    in do
      treeCtrlDeleteAllItems tc
      root <- treeCtrlAddRoot tc "Organizations" 0 0 objectNull
      mapM_ (populateOrg root) $ dOrganizations doc
      treeCtrlExpand tc root
      treeCtrlGetNextVisible tc root >>= treeCtrlSelectItem tc

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
