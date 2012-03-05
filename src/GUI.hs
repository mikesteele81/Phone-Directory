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

module GUI
    ( mainWindow
    ) where

import Control.Applicative
import Control.Monad as M
import Data.Char
import qualified Data.Function as F (on)
import Data.List
import Data.Time

import Control.Monad.IO.Class
import Control.Monad.Trans.Error
import qualified Data.Aeson as A
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Graphics.UI.WX as WX
import Graphics.UI.WXCore hiding (Document)
import System.FilePath
import qualified Text.CSV.ByteString as CSV

import ContactInfo as CI
import Document
import Export
import GUIConstants
import Name as N
import qualified Organization as O
import PageProperties (mkPageProperties, PageProperties)
import PageSetupGUI
import Priority

type WXError a = ErrorT String IO a

-- |The application only exports to .pdf.  I could see other formats like
-- .html being useful to.
exportTypesSelection :: [(String, [String])]
exportTypesSelection = [ ("PDF", ["*.pdf"])
                       , ("Comma-Separated-Value", ["*.csv"])]

-- |The application only exports to .pdf.  I could see other formats like
-- .html being useful to.
importTypesSelection :: [(String, [String])]
importTypesSelection = [ ("Comma-Separated-Value", ["*.csv"])]

-- |This is the format to be saved in.  It's a Shame that the Haskell YAML
-- library was made available a week after I settled on this.
openSaveTypesSelection :: [(String, [String])]
openSaveTypesSelection = [("Phone Directory (*.pdir)", ["*.pdir"])]

-- |When you first start the application this is the filename chosen to save
-- to.
defaultFile :: String
defaultFile = "untitled.pdir"

-- |Text blurb that goes in the about box.
aboutTxt :: Text
aboutTxt =
    "PhoneDirectory 0.7\n\
    \Copyright (C) 2009 Michael Steele\n\n\
    \This program comes with ABSOLUTELY NO WARRANTY; for\n\
    \details go to http://github.com/mikesteele81/Phone-Directory/.\n\n\
    \This is free software, and you are welcome to\n\
    \redistribute it under certain conditions; read the\n\
    \included license file for details."

-- |Build the main window and start the event loop.
mainWindow :: Maybe String -> IO ()
mainWindow filename = do
  file       <- varCreate defaultFile
  modified   <- varCreate False
  properties <- varCreate mkPageProperties

  f  <- frame            []

  pRight  <- panel    f     []

  mFile   <- menuPane        []
  iNew    <- menuItem mFile  []
  iOpen   <- menuItem mFile  []
  iSave   <- menuItem mFile  []
  iSaveAs <- menuItem mFile  []
  menuLine mFile
  iPage   <- menuItem mFile  []
  iImport <- menuItem mFile  []
  iExport <- menuItem mFile  []
  menuLine mFile
  iQuit   <- menuQuit mFile  []

  mHelp   <- menuHelp        []
  iAbout  <- menuAbout mHelp []

  lFirst    <- staticText pRight
      [ text := "First Name:"
      , tooltip := "Enter the contact's first name.  If the contact \
                   \only goes by a single name, enter it either \
                   \here or in the last name field."]
  eFirst    <- entry pRight []
  lLast     <- staticText pRight
      [ text := "Last Name:"
      , tooltip := "Enter the contact's last name.  If the contact \
                   \only goes by a single name, enter it either \
                   \here or in the first name field."]
  eLast     <- entry pRight []
  lPhone    <- staticText pRight
      [ text := "Phone Number:"
      , tooltip := "Enter thecontact's phone number."]
  ePhone    <- entry pRight []
  lPriority <- staticText pRight
      [ text := "Priority:"
      , tooltip := "Low values will sort before contacts with \
                   \higher values."]
  ePriority <- spinCtrl pRight (fromEnum (minBound :: Priority))
                               (fromEnum (maxBound :: Priority)) []

  tc <- treeCtrl f []

  -- Use to determine scaling based on a standard 96dpi view.
  Size xDpi yDpi <- bracket (clientDCCreate f) clientDCDelete
                    dcGetPPI

  let
      onTreeEvent (TreeSelChanged itm' itm) | treeItemIsOk itm' =
          trapError $ do
              -- Delete non-root nodes without a name
              root <- liftIO $ treeCtrlGetRootItem tc
              M.when (root /= itm && treeItemIsOk itm) $ do
                  ci <- right2CI
                  M.when (T.null . CI.renderWith lastFirst $ ci)
                      (liftIO $ treeCtrlDelete tc itm)
              if root == itm' then clearDisableDetails
                else treeItem2CI tc itm' >>= updateDetails

              liftIO propagateEvent

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
                (ContactInfo (mkPriority 1) (mkName "" "") "")
            setModified True
            treeCtrlSelectItem tc itm'
            windowSetFocus eFirst
          KeyDelete -> unless (root == itm) $ do
              n <- treeCtrlGetNextSibling tc itm
              p <- treeCtrlGetPrevSibling tc itm
              u <- treeCtrlGetParent tc itm
              treeCtrlDelete tc itm
              if treeItemIsOk n
                then treeCtrlSelectItem tc n
                else if treeItemIsOk p
                       then treeCtrlSelectItem tc p
                       else if treeItemIsOk u
                              then treeCtrlSelectItem tc u
                              else treeCtrlSelectItem tc root
              setModified True
          _         -> return ()
        propagateEvent
      onTreeEvent _ = propagateEvent
      setModified m = varSet modified m >> updateTitle

      updateTitle = do
          fn <- varGet file
          m <- varGet modified
          set f [WX.text := title m fn]

      checkConfirmUnsaved :: WXError () -> WXError ()
      checkConfirmUnsaved op = do
          m <- liftIO $ varGet modified
          if m
            then do
              confirmed <- liftIO $ confirmDialog f caption msg False
              M.when confirmed op
            else op
        where
          caption = "Unsaved Changes"
          msg = "You have unsaved changes. Are you sure you want to continue?"

      right2CI :: WXError ContactInfo
      right2CI = liftIO $ do
          firstName <- get eFirst    WX.text
          lastName  <- get eLast     WX.text
          phone     <- get ePhone    WX.text
          priority  <- get ePriority WX.selection
          return ContactInfo
              { cName     = mkName (T.pack firstName) (T.pack lastName)
              , cPhone    = T.pack phone
              , cPriority = mkPriority priority }

      updateDetails :: ContactInfo -> WXError ()
      updateDetails ci = liftIO $ do
        set eFirst [ enabled := True, WX.text := T.unpack . N.given $ n]
        set eLast  [ enabled := True, WX.text := maybe "" T.unpack $ N.sur n]
        set ePhone     [ enabled := True, WX.text := T.unpack (cPhone ci) ]
        set ePriority  [ enabled := True, WX.selection := fromEnum $ cPriority ci ]
        where n = cName ci

      clearDisableDetails :: WXError ()
      clearDisableDetails = liftIO $ do
        set eFirst    [ enabled := False, WX.text := "" ]
        set eLast     [ enabled := False, WX.text := "" ]
        set ePhone    [ enabled := False, WX.text := "" ]
        set ePriority [ enabled := False, WX.text := "" ]

      trapError :: WXError a -> IO ()
      trapError x = do
          e <- runErrorT x
          either (errorDialog f "error") (const $ return ()) e

      handleFocus :: Bool -> WXError ()
      -- lost focus
      handleFocus False = do
          itm <- liftIO $ treeCtrlGetSelection tc
          ci <- treeItem2CI tc itm
          ci' <- right2CI
          M.when (ci /= ci') $ do
              updateNode tc itm ci'
              liftIO $ setModified True
      handleFocus _ = return ()

      commitStringInput :: TextCtrl a -> Bool -> WXError ()
      commitStringInput ctrl hasFocus = do
          liftIO $ set ctrl [WX.text :~ unpad]
          handleFocus hasFocus

      commitPriorityInput :: SpinCtrl a -> Bool -> WXError ()
      commitPriorityInput ctrl hasFocus = do
          liftIO $ set ctrl [ selection :~ fromEnum . mkPriority ]
          handleFocus hasFocus

      new :: WXError ()
      new = do
          -- this prevents events from firing.
          clearDisableDetails
          populateTree tc mkDocument
          liftIO $ do
              varSet file defaultFile
              setModified False
              windowSetFocus tc

      importFile :: FilePath -> WXError ()
      importFile fp = do
          doc <- importCSV fp
          -- this prevents events from firing.
          clearDisableDetails
          populateTree tc doc
          liftIO $ do
              --importing doesn't imply that you can 'save' to the same file.
              varSet file defaultFile
              --importing implies that the source had incomplete information.
              setModified True
              varSet properties (pageProperties doc)

      open :: FilePath -> WXError ()
      open fp = do
          doc <- loadDoc fp
          -- this prevents events from firing.
          clearDisableDetails
          populateTree tc doc
          liftIO $ do
              varSet file fp
              setModified False
              varSet properties (pageProperties doc)

      save :: FilePath -> WXError ()
      save fp = do
          props <- liftIO $ varGet properties
          props2Doc <- tree2Doc tc
          let
              doc  = props2Doc props
              doc' = sortDoc lastFirst doc
          saveDoc fp doc'
          M.when (doc /= doc') $ do
              clearDisableDetails
              populateTree tc doc'
          liftIO $ varSet file fp
          liftIO $ setModified False

  set mFile  [ WX.text := "&File"]
  set iNew   [ WX.text := "&New"
             , on command := trapError $ checkConfirmUnsaved new
             ]

  set iOpen
      [ WX.text := "&Open..."
      , on command := trapError $ checkConfirmUnsaved $ do
          name <- liftIO $ fileOpenDialog f True True "Open phone directory"
              openSaveTypesSelection "" ""
          maybe (return ()) open name
      ]

  set iSave [ WX.text := "&Save"
            , on command := trapError $ liftIO (varGet file) >>= save
            ]

  set iSaveAs
      [ WX.text := "Save &As..."
      , on command := trapError $ do
          name <- liftIO $ fileSaveDialog f True True "Save phone directory"
              openSaveTypesSelection "" ""
          maybe (return ()) save name
      ]

  set iPage
      [ WX.text := "Page Setup..."
      , on command := trapError $ do
          prop <- liftIO $ varGet properties
          prop' <- liftIO $ PageSetupGUI.pageSetupDialog f prop
          maybe (return ()) (\p -> unless (prop == p) . liftIO $ do
              varSet properties p
              setModified True) prop'
          return ()
      ]

  set iImport
      [ WX.text := "&Import..."
      , on command := trapError $ checkConfirmUnsaved $ do
          name <- liftIO $ fileOpenDialog f True True "Import phone directory"
              importTypesSelection "" ""
          maybe (return ()) importFile name
      ]

  set iExport
      [ WX.text := "Ex&port..."
      , on command := do
          name <- fileSaveDialog f True True "Export phone directory"
                  exportTypesSelection "" ""
          case name of
            Just name' -> trapError $ do
                props <- liftIO $ varGet properties
                op <- tree2Doc tc
                let ext = drop (length name' - 3) (map toLower name')
                if ext == "pdf"
                  then generate name' (op props)
                  else M.when (ext == "csv") . liftIO $ S.writeFile name'
                           (T.encodeUtf8 . printCSV . toCSVRecords $ op props)
            Nothing -> return ()
      ]

  -- The 'closing' event handler checks for unsaved changes.
  set iQuit  [ on command := close f ]
  set iAbout [ on command := infoDialog f "About Phone Directory" (T.unpack aboutTxt) ]

  set eFirst    [ processEnter := True
                , on command   := trapError $ commitStringInput eFirst False
                , on focus     := trapError . commitStringInput eFirst
                ]
  set eLast     [ processEnter := True
                , on command   := trapError $ commitStringInput eLast False
                , on focus     := trapError . commitStringInput eLast
                ]
  set ePhone    [ processEnter := True
                , on command   := trapError $ commitStringInput ePhone False
                , on focus     := trapError . commitStringInput ePhone
                ]
  set ePriority [ on select := trapError $ commitPriorityInput ePriority False
                , on focus  := trapError . commitPriorityInput ePriority
                ]

  set tc [ on treeEvent := onTreeEvent ]

  set pRight [ layout := column (ctrlPadding yDpi)
                 . map (column (lblPadding yDpi) . map WX.hfill)
                 $ [ [widget lFirst, widget eFirst]
                   , [widget lLast, widget eLast]
                   , [widget lPhone, widget ePhone]
                   , [widget lPriority, widget ePriority]
                   ]
             ]

  -- f must have its layout defined last. Otherwise things don't layout
  -- properly until the main application window is resized.
  set f [ on closing := trapError . checkConfirmUnsaved . liftIO
            $ void (windowDestroy f)
        , menuBar    := [mFile, mHelp]
        , picture    := T.unpack "data/images/pdirectory.ico"
        , layout     := margin (winPadding xDpi) $ WX.fill
            $ row (ctrlPadding xDpi)
            [ WX.fill $ widget tc, WX.fill $ widget pRight ]
        , clientSize := sz (scale 640 xDpi) (scale 480 yDpi)
        ]

  --name has already been set.  make a new one in case opening a file fails.
  trapError new
  windowSetFocus tc

  case filename of
      Nothing -> return ()
      Just fn -> trapError $ open fn

-- |Scrap and rebuild the heirarchical tree.  Once this is done, expand it and
-- select the first non-root node.
populateTree :: TreeCtrl a -> Document -> WXError ()
populateTree tc doc =
    liftIO $ do
        treeCtrlDeleteAllItems tc
        root <- treeCtrlAddRoot tc "Organizations" 0 0 objectNull
        mapM_ (populateOrg root) $ dOrganizations doc
        --Select root first.  That way if there are no child nodes at least
        --something gets selected.
        if null (dOrganizations doc)
          then treeCtrlSelectItem tc root
          else do
            treeCtrlExpand tc root
            treeCtrlGetNextVisible tc root >>= treeCtrlSelectItem tc
  where
    addItem p itm = do
        tc' <- treeCtrlAppendItem tc p
            (T.unpack . CI.renderWith lastFirst $ itm) 0 0 objectNull
        treeCtrlSetItemClientData tc tc' (return ()) itm
        return tc'
    populateOrg p org = do
        orgTc <- addItem p $ O.oInfo org
        mapM_ (addItem orgTc) $ O.oContacts org

-- |Set the supplied frame's title bar based on the supplied file.
title
    :: Bool      -- ^Modifications?
    -> FilePath  -- ^File name
    -> String
title m = (if m then ("* " ++) else id) . (++ " - Phone Directory") . takeBaseName

-- |Create a Document object based on the tree heirarchy.
tree2Doc :: TreeCtrl a -> WXError (PageProperties -> Document)
tree2Doc tc = do
    root <- liftIO $ treeCtrlGetRootItem tc
    orgs <- liftIO $ treeCtrlWithChildren tc root $ \itm ->
        runErrorT $ do
            orgCI <- treeItem2CI tc itm
            contacts <- liftIO $ treeCtrlWithChildren tc itm
                $ runErrorT . treeItem2CI tc
            either throwError return $ O.Organization orgCI <$> sequence contacts
    (year, month, day) <- liftIO $ liftM
        (toGregorian . localDay . zonedTimeToLocalTime) getZonedTime
    either throwError return $ Document
        (show month ++ "/" ++ show day ++ "/" ++ show year)
        <$> sequence orgs

-- |Create a ContactInfo by pulling it from the tree heirarchy.  This is the
-- only place where 'unsafeTreeCtrlGetItemClientData' should be called.
treeItem2CI :: TreeCtrl a -> TreeItem -> WXError ContactInfo
treeItem2CI tc itm = do
    -- If this fails, it will probably raise a segmentation fault.
    ci <- liftIO $ unsafeTreeCtrlGetItemClientData tc itm
    maybe (throwError "Tree node does not contain contact information") return ci

-- |Save the supplied document to a file.
saveDoc :: FilePath -> Document -> WXError ()
saveDoc fp doc =
    (liftIO . L.writeFile fp . A.encode $ doc)
    `catchError` (throwError . msg)
  where
    msg x = "Failed to save directory to " ++ fp ++ ": " ++ x

-- |Attempt to load a document from the supplied file.
loadDoc :: FilePath -> WXError Document
loadDoc fp = ( do
    s <- liftIO $ L.readFile fp
    maybe (throwError msg) return $ A.decode s
    ) `catchError` (throwError . (\e -> msg ++ ": " ++ e))
  where
    msg = "Failed to load " ++ fp

-- |Attempt to load a document from the supplied file.
importCSV :: FilePath -> WXError Document
importCSV fp = ( do
        bs <- liftIO . S.readFile $ fp
        maybe (throwError "Error Parsing CSV file.") go $ CSV.parseCSV bs
    ) `catchError` (throwError . msg)
  where
    go csv = do
      orgs <- either (throwError . T.unpack) return (O.fromCSV csv)
      return $ mkDocument {dOrganizations = O.mergeOrgs orgs}
    msg x = "Failed to load " ++ fp ++ ": " ++ x

unpad :: String -> String
unpad s = case tail . groupBy ((==) `F.on` isSpace) . (" " ++) . (++ " ") $ s of
            [] -> []
            s' -> join . init $ s'

-- | update left side to match what's entered on right
updateNode :: TreeCtrl a
           -> TreeItem
           -> ContactInfo
           -> WXError ()
updateNode tc itm ci = liftIO $ do
    -- Never update the root node.
    root <- treeCtrlGetRootItem tc
    M.when (root /= itm && treeItemIsOk itm) $ do
        treeCtrlSetItemClientData tc itm (return ()) ci
        treeCtrlSetItemText tc itm
            (T.unpack . CI.renderWith N.lastFirst $ ci)

-- | Given an object of type CSV, generate a CSV formatted
-- string. Always uses escaped fields.
printCSV :: CSV.CSV -> Text
printCSV records = unlines (printRecord `map` records) `T.append` "\n"
    where printRecord = T.concat . intersperse "," . map printField
          printField f = "\"" `T.append` T.concatMap escape (T.decodeUtf8 f)
                         `T.append` "\""
          escape '"' = "\"\""
          escape x = T.singleton x
          unlines = T.concat . intersperse "\n"
