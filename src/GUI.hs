{- This file is part of PhoneDirectory.

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

module GUI
    where

import Control.Applicative
import Control.Monad
import Data.Time
import Graphics.UI.WX as WX
import Graphics.UI.WXCore hiding ( Document )
import System.FilePath
import System.IO
import Text.JSON
import Text.JSON.Pretty

import ContactInfo
import Document
import Name
import Organization
import PDF

exportTypesSelection :: [(String, [String])]
exportTypesSelection = [("PDF", ["*.pdf"])]

fileTypesSelection :: [(String, [String])]
fileTypesSelection = [("JSON", ["*.json"])]

defaultFile :: String
defaultFile = "untitled.json"

edit :: IO ()
edit = do
  file <- varCreate defaultFile

  f  <- frame            []
  sw <- splitterWindow f []
  
  pLeft   <- panel    sw     []
  pRight  <- panel    sw     []
  
  mFile   <- menuPane        []
  iNew    <- menuItem mFile  []
  iOpen   <- menuItem mFile  []
  iSave   <- menuItem mFile  []
  iSaveAs <- menuItem mFile  []
  iExport <- menuItem mFile  []
  ()      <- menuLine mFile
  iQuit   <- menuQuit mFile  []
  mHelp   <- menuHelp        []
  iAbout  <- menuAbout mHelp []
  
  tFirst    <- staticText pRight [ WX.text := "First Name:"   ]
  tLast     <- staticText pRight [ WX.text := "Last Name:"    ]
  tPhone    <- staticText pRight [ WX.text := "Phone Number:" ]
  tPriority <- staticText pRight [ WX.text := "Priority:"     ]

  eFirst    <- entry pRight []
  eLast     <- entry pRight []
  ePhone    <- entry pRight []
  ePriority <- entry pRight []

  tc <- treeCtrl pLeft []
  
  let
      onTreeEvent (TreeSelChanged itm' itm) | treeItemIsOk itm' = do
        -- Delete non-root nodes without a name
        root <- treeCtrlGetRootItem tc
        Control.Monad.when (root /= itm && treeItemIsOk itm) $ do
          ci <- right2CI
          case show ci of
            "" -> treeCtrlDelete tc itm
            _  -> return ()
        
        ci <- getTreeItem itm'
        maybe clearDisableDetails updateDetails ci

        propagateEvent
      onTreeEvent (TreeKeyDown _ (EventKey k _ _)) = do
        -- TreeKeyDown's item member doesn't hold anything.
        itm <- treeCtrlGetSelection tc
        root <- treeCtrlGetRootItem tc
        case k of
          KeyInsert -> do
            itmP <- treeCtrlGetParent tc itm
            -- We only want the heirarchy 2 deep
            let p = if root == itmP || root == itm then itm else itmP

            itm' <- treeCtrlAppendItem tc p "<New Item>" 0 0 objectNull
            treeCtrlSetItemClientData tc itm' (return ())
                (ContactInfo (SingleName "") "" 1)
            treeCtrlSelectItem tc itm'
            windowSetFocus eFirst
          KeyDelete -> unless (root == itm) $ treeCtrlDelete tc itm
          _         -> return ()
        propagateEvent
      onTreeEvent _ = propagateEvent
      
      right2CI :: IO (ContactInfo Name)
      right2CI = do
        firstName <- get eFirst    WX.text
        lastName  <- get eLast     WX.text
        phone     <- get ePhone    WX.text
        priority  <- get ePriority WX.text
        let name = case (firstName, lastName) of
                     ("", n) -> SingleName n
                     (n, "") -> SingleName n
                     (f', l) -> FirstLast f' l
        return ContactInfo
                 { cName     = name
                 , cPhone    = phone
                 , cPriority = read priority
                 }

      -- | update left side to match what's entered on right
      updateTreeItem :: (Show a) => TreeItem -> ContactInfo a -> IO ()
      updateTreeItem itm ci = do
        treeCtrlSetItemClientData tc itm (return ()) ci
        treeCtrlSetItemText tc itm $ show ci

      getTreeItem :: TreeItem -> IO (Maybe (ContactInfo Name))
      getTreeItem = unsafeTreeCtrlGetItemClientData tc

      updateDetails :: ContactInfo Name -> IO ()
      updateDetails ci = do
        case cName ci of
          FirstLast first l -> do
            set eFirst [ enabled := True, WX.text := first ]
            set eLast  [ enabled := True, WX.text := l     ]
          SingleName n -> do
            set eFirst [ enabled := True, WX.text := n  ]
            set eLast  [ enabled := True, WX.text := "" ]
        set ePhone     [ enabled := True, WX.text := cPhone ci ]
        set ePriority  [ enabled := True, WX.text := show $ cPriority ci ]

      clearDisableDetails :: IO ()
      clearDisableDetails = do
        set eFirst    [ enabled := False, WX.text := "" ]
        set eLast     [ enabled := False, WX.text := "" ]
        set ePhone    [ enabled := False, WX.text := "" ]
        set ePriority [ enabled := False, WX.text := "" ]

      updateTitle :: IO ()
      updateTitle = do
        fn <- varGet file
        set f [WX.text := takeBaseName fn ++ " - Phone Directory"]

      handleInputFocusChanged :: Bool -> IO ()
      handleInputFocusChanged False = do
        ci <- right2CI
        itm <- treeCtrlGetSelection tc
        updateTreeItem itm ci
      handleInputFocusChanged _ = return ()

  set mFile  [ WX.text := "&File"    ]
  set iNew   [ WX.text := "&New"     ]
  set iOpen  [ WX.text := "&Open...", on command := do
                 name <- fileOpenDialog f True True "Open phone directory"
                         fileTypesSelection "" ""
                 case name of
                   Just name' -> do
                     doc' <- load name'
                     case doc' of
                       Just doc'' -> do
                                   putStrLn $ "Opening " ++ name'
                                   populateTree tc doc''
                       Nothing -> return ()
                     varSet file name'
                     updateTitle
                   Nothing -> return ()
             ]
  set iSave  [ WX.text := "&Save", on command := do
                 doc' <- tree2Doc tc
                 case doc' of
                   Just doc'' -> do
                     file' <- varGet file
                     save file' doc''
                   Nothing -> putStrLn "bad doc" >> return ()
             ]
  set iSaveAs  [ WX.text := "Save &As...", on command := do
                   name <- fileSaveDialog f True True "Save phone directory"
                           fileTypesSelection "" ""
                   case name of
                     Just name' -> do
                       doc' <- tree2Doc tc
                       case doc' of
                         Just doc'' -> save name' doc''
                         Nothing -> return ()
                       varSet file name'
                       updateTitle
                     Nothing -> return ()
               ]

  set iExport [ WX.text := "Ex&port...", on command := do
                  name <- fileSaveDialog f True True "Export phone directory"
                          exportTypesSelection "" ""
                  case name of
                    Just name' -> do
                      doc' <- tree2Doc tc
                      case doc' of
                        Just doc'' -> generate doc'' name'
                        Nothing    -> return ()
                    Nothing -> return ()
              ]

  set iQuit  [ on command := close f ]
  set iAbout [ on command := infoDialog f "About Phone Directory" "test" ]

  set eFirst    [ on focus := handleInputFocusChanged ]
  set eLast     [ on focus := handleInputFocusChanged ]
  set ePhone    [ on focus := handleInputFocusChanged ]
  set ePriority [ on focus := handleInputFocusChanged ]

  set tc [ on treeEvent := onTreeEvent ]
  
  set pLeft [ layout := WX.fill $ widget tc ]
  set pRight [ layout := margin 6 $ column 5
                          [ widget tFirst, widget eFirst
                          , widget tLast, widget eLast
                          , widget tPhone, widget ePhone
                          , widget tPriority, widget ePriority
                          ]
             ]
  populateTree tc (mkDocument :: Document Name)
  updateTitle

  set f [ menuBar    := [mFile, mHelp]
        , layout     := WX.fill $ vsplit sw 5 200 (widget pLeft) (widget pRight)
        , clientSize := sz 640 480
        ]

  windowSetFocus tc

populateTree :: (Show b) => TreeCtrl a -> Document b -> IO ()
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
    contacts <- treeCtrlWithChildren tc itm
      $ unsafeTreeCtrlGetItemClientData tc
    return $ Organization <$> orgCI <*> sequence contacts
  time <- getCurrentTime
  let (year, month, day) = toGregorian $ utctDay time
  return $ Document (show month ++ "/" ++ show day ++ "/" ++ show year)
        <$> sequence orgs

save :: FilePath -> Document Name -> IO ()
save file = writeFile file . show . pp_value . showJSON

load :: FilePath -> IO (Maybe (Document Name))
load fp =
  withFile fp ReadMode $ \h -> do
            input <- hGetContents h
            case decodeStrict input >>= readJSON of
              Error s -> putStrLn ("Error: " ++ s) >> return Nothing
              Ok doc -> return $ Just doc
