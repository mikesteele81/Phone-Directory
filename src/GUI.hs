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

module GUI
    ( mainWindow
    ) where

import Control.Applicative
import Control.Monad as M
import Control.Monad.Error (catchError, throwError)
import Data.Char
import qualified Data.Function as F (on)
import Data.List
import Data.Time
import Graphics.UI.WX as WX
import Graphics.UI.WXCore hiding (Document)
import System.FilePath
import System.IO
import Text.JSON
import Text.JSON.Pretty

import ContactInfo
import Document
import Export
import Name
import Organization
import Priority
import WXError

-- |The number of pixels between controls that are grouped together.
ctrlPadding :: Int
ctrlPadding = 4

-- |The number of pixels between controls and their labels.
lblPadding :: Int
lblPadding = 3

-- |The number of pixels between the window border or vertical/horizontal
-- spacers and internal controls.
winPadding :: Int
winPadding = 7

-- |The application only exports to .pdf.  I could see other formats like
-- .html being useful to.
exportTypesSelection :: [(String, [String])]
exportTypesSelection = [("PDF (*.pdf)", ["*.pdf"])]

-- |This is the format to be saved in.  It's a Shame that the Haskell YAML
-- library was made available a week after I settled on this.
fileTypesSelection :: [(String, [String])]
fileTypesSelection = [("Phone Directory (*.pdir)", ["*.pdir"])]

-- |When you first start the application this is the filename chosen to save
-- to.
defaultFile :: String
defaultFile = "untitled.pdir"

-- |Text blurb that goes in the about box.
aboutTxt :: String
aboutTxt =
    "PhoneDirectory Copyright (C) 2009 Michael Steele\n\n\
    \This program comes with ABSOLUTELY NO WARRANTY; for\n\
    \details go to http://www.michaelsteele.us/phonedirectory/.\n\n\
    \This is free software, and you are welcome to\n\
    \redistribute it under certain conditions; read the\n\
    \included license file for details."

-- |Build the main window and start the event loop.
mainWindow :: Maybe String -> IO ()
mainWindow filename = do
  file     <- varCreate defaultFile
  modified <- varCreate False

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
  
  eFirst    <- entry pRight []
  eLast     <- entry pRight []
  ePhone    <- entry pRight []
  ePriority <- spinCtrl pRight (fromEnum (minBound :: Priority))
                               (fromEnum (maxBound :: Priority)) []

  tc <- treeCtrl pLeft []
  
  let
      onTreeEvent (TreeSelChanged itm' itm) | treeItemIsOk itm' =
          trapError $ do
              -- Delete non-root nodes without a name
              root <- liftIO $ treeCtrlGetRootItem tc
              M.when (root /= itm && treeItemIsOk itm) $ do
                  ci <- right2CI
                  case show ci of
                      "" -> liftIO $ treeCtrlDelete tc itm
                      _  -> return ()
 
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
              treeCtrlDelete tc itm
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
          msg = "You have unsaved changes.  Are you sure you want to continue?"

      right2CI :: WXError (ContactInfo Name)
      right2CI = liftIO $ do
          firstName <- get eFirst    WX.text
          lastName  <- get eLast     WX.text
          phone     <- get ePhone    WX.text
          priority  <- get ePriority WX.selection
          return ContactInfo
              { cName     = mkName firstName lastName
              , cPhone    = phone
              , cPriority = mkPriority priority }

      updateDetails :: ContactInfo Name -> WXError ()
      updateDetails ci = liftIO $ do
        case cName ci of
          FirstLast first l -> do
            set eFirst [ enabled := True, WX.text := first ]
            set eLast  [ enabled := True, WX.text := l     ]
          SingleName n -> do
            set eFirst [ enabled := True, WX.text := n  ]
            set eLast  [ enabled := True, WX.text := "" ]
        set ePhone     [ enabled := True, WX.text := cPhone ci ]
        set ePriority  [ enabled := True, WX.selection := fromEnum $ cPriority ci ]

      clearDisableDetails :: WXError ()
      clearDisableDetails = liftIO $ do
        set eFirst    [ enabled := False, WX.text := "" ]
        set eLast     [ enabled := False, WX.text := "" ]
        set ePhone    [ enabled := False, WX.text := "" ]
        set ePriority [ enabled := False, WX.text := "" ]

      trapError :: WXError a -> IO ()
      trapError x = do
          x' <- wxerror x
          case x' of
              Left y -> errorDialog f "error" y
              Right _ -> return ()

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
          populateTree tc (mkDocument :: Document Name)
          liftIO $ do
              varSet file defaultFile
              setModified False
              windowSetFocus tc

      open :: FilePath -> WXError ()
      open fp = do
          doc <- loadDoc fp
          -- this prevents events from firing.
          clearDisableDetails
          populateTree tc doc
          liftIO $ do
              varSet file fp
              setModified False

      save :: FilePath -> WXError ()
      save fp = do
          doc <- tree2Doc tc
          let doc' = sortDoc doc
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
              fileTypesSelection "" ""
          maybe (return ()) open name
      ]

  set iSave [ WX.text := "&Save"
            , on command := trapError $ liftIO (varGet file) >>= save
            ]

  set iSaveAs
      [ WX.text := "Save &As..."
      , on command := trapError $ do
          name <- liftIO $ fileSaveDialog f True True "Save phone directory"
              fileTypesSelection "" ""
          maybe (return ()) save name
      ]

  set iExport
      [ WX.text := "Ex&port..."
      , on command := do
          name <- fileSaveDialog f True True "Export phone directory"
                  exportTypesSelection "" ""
          case name of
            Just name' -> trapError $ tree2Doc tc >>= generate name'
            Nothing -> return ()
      ]

  set iQuit  [ on command := trapError $ checkConfirmUnsaved $ liftIO $ close f ]
  set iAbout [ on command := infoDialog f "About Phone Directory" aboutTxt ]

  set eFirst    [ processEnter := True
                , on command   := trapError $ commitStringInput eFirst False
                , on focus     := trapError . commitStringInput eFirst
                , tooltip := "Enter the contact's first name.  If the contact \
                             \only goes by a single name, enter it either \
                             \here or in the last name field."]
  set eLast     [ processEnter := True
                , on command   := trapError $ commitStringInput eLast False
                , on focus     := trapError . commitStringInput eLast
                , tooltip := "Enter the contact's last name.  If the contact \
                             \only goes by a single name, enter it either \
                             \here or in the first name field."]
  set ePhone    [ processEnter := True
                , on command   := trapError $ commitStringInput ePhone False
                , on focus     := trapError . commitStringInput ePhone
                , tooltip := "Enter thecontact's phone number." ]
  set ePriority [ on select := trapError $ commitPriorityInput ePriority False
                , on focus  := trapError . commitPriorityInput ePriority
                , tooltip := "Low values will sort before contacts with \
                             \higher values." ]

  set tc [ on treeEvent := onTreeEvent ]

  set f [ menuBar    := [mFile, mHelp]
        , picture    := "data/images/pdirectory.ico"
        , layout     := WX.fill $ margin winPadding $ vsplit sw winPadding 200
                        (widget pLeft) (widget pRight)
        , clientSize := sz 640 480
        ]

  set pLeft [ layout := WX.fill $ widget tc ]
  set pRight
      [ layout := column ctrlPadding
          [ labeled "First Name:"   $ widget eFirst
          , labeled "Last Name:"    $ widget eLast
          , labeled "Phone Number:" $ widget ePhone
          , labeled "Priority:"     $ widget ePriority ]
      ]

  --name has already been set.  make a new one in case opening a file fails.
  trapError new
  windowSetFocus tc

  case filename of
      Nothing -> return ()
      Just fn -> trapError $ open fn

-- |Scrap and rebuild the heirarchical tree.  Once this is done, expand it and
-- select the first non-root node.
populateTree :: (Show b) => TreeCtrl a -> Document b -> WXError ()
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
        tc' <- treeCtrlAppendItem tc p (show itm) 0 0 objectNull
        treeCtrlSetItemClientData tc tc' (return ()) itm
        return tc'
    populateOrg p org = do
        orgTc <- addItem p $ oInfo org
        mapM_ (addItem orgTc) $ oContacts org

-- |Set the supplied frame's title bar based on the supplied file.
title
    :: Bool      -- ^Modifications?
    -> FilePath  -- ^File name
    -> String
title m = (if m then ("* " ++) else id) . (++ " - Phone Directory") . takeBaseName

-- |Create a Document object based on the tree heirarchy.
tree2Doc :: TreeCtrl a -> WXError (Document (ContactInfo Name))
tree2Doc tc = do
    root <- liftIO $ treeCtrlGetRootItem tc
    orgs <- liftIO $ treeCtrlWithChildren tc root $ \itm ->
        wxerror $ do
            orgCI <- treeItem2CI tc itm
            contacts <- liftIO $ treeCtrlWithChildren tc itm
                $ wxerror . treeItem2CI tc
            fromEither $ Organization orgCI <$> sequence contacts
    time <- liftIO getCurrentTime
    let (year, month, day) = toGregorian $ utctDay time
        date = show month ++ "/" ++ show day ++ "/" ++ show year
    fromEither $ Document date <$> sequence orgs

-- |Create a ContactInfo by pulling it from the tree heirarchy.  This is the
-- only place where 'unsafeTreeCtrlGetItemClientData' should be called.
treeItem2CI :: TreeCtrl a -> TreeItem -> WXError (ContactInfo Name)
treeItem2CI tc itm = do
    -- If this fails, it will probably raise a segmentation fault.
    ci <- liftIO $ unsafeTreeCtrlGetItemClientData tc itm
    fromMaybe "Tree node does not contain contact information" ci

-- |Save the supplied document to a file.
saveDoc :: FilePath -> Document (ContactInfo Name) -> WXError ()
saveDoc fp doc =
    (liftIO . writeFile fp . show . pp_value . showJSON $ doc)
    `catchError` (throwError . (msg ++))
  where
    msg = "Failed to save directory to " ++ fp ++ ":\n"

-- |Attempt to load a document from the supplied file.
loadDoc :: FilePath -> WXError (Document (ContactInfo Name))
loadDoc fp = ( do
        s <- liftIO $ readFile fp
        fromJSONResult $ decodeStrict s >>= readJSON
    ) `catchError` (throwError . (msg ++))
  where
    msg = "Failed to load " ++ fp ++ ":\n"

-- |Combinator that lays out the first argument directly over the second.
labeled :: String -- ^Label to use.
    -> Layout     -- ^Thing to get a label.
    -> Layout
labeled s l = column lblPadding [label s, l]

unpad :: String -> String
unpad s = case tail . groupBy ((==) `F.on` isSpace) . (" " ++) . (++ " ") $ s of
            [] -> []
            s' -> join . init $ s'

-- | update left side to match what's entered on right
updateNode :: (Show b)
    => TreeCtrl a
    -> TreeItem
    -> ContactInfo b
    -> WXError ()
updateNode tc itm ci = liftIO $ do
    -- Never update the root node.
    root <- treeCtrlGetRootItem tc
    M.when (root /= itm && treeItemIsOk itm) $ do
        treeCtrlSetItemClientData tc itm (return ()) ci
        treeCtrlSetItemText tc itm $ show ci

