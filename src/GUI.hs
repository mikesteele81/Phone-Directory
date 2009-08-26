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
import qualified Control.Monad as M
import Control.Monad.Error
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
mainWindow :: IO ()
mainWindow = do
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
  
  eFirst    <- entry pRight []
  eLast     <- entry pRight []
  ePhone    <- entry pRight []
  ePriority <- spinCtrl pRight (fromEnum (minBound :: Priority))
                               (fromEnum (maxBound :: Priority)) []

  tc <- treeCtrl pLeft []
  
  let
      onTreeEvent (TreeSelChanged itm' itm) | treeItemIsOk itm' =
          runErrorT ( do
              -- Delete non-root nodes without a name
              root <- fromIO Nothing $ treeCtrlGetRootItem tc
              M.when (root /= itm && treeItemIsOk itm) $ do
                  ci <- right2CI
                  case show ci of
                      "" -> fromIO Nothing $ treeCtrlDelete tc itm
                      _  -> return ()
 
              case root == itm' of
                True  -> clearDisableDetails
                False -> treeItem2CI tc itm' >>= updateDetails

              fromIO Nothing $ propagateEvent
          ) >>= trapError

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
            treeCtrlSelectItem tc itm'
            windowSetFocus eFirst
          KeyDelete -> unless (root == itm) $ treeCtrlDelete tc itm
          _         -> return ()
        propagateEvent
      onTreeEvent _ = propagateEvent
      
      right2CI :: WXError (ContactInfo Name)
      right2CI = fromIO Nothing $ do
          firstName <- get eFirst    WX.text
          lastName  <- get eLast     WX.text
          phone     <- get ePhone    WX.text
          priority  <- get ePriority WX.selection
          return ContactInfo
              { cName     = mkName firstName lastName
              , cPhone    = phone
              , cPriority = mkPriority priority }

      updateDetails :: ContactInfo Name -> WXError ()
      updateDetails ci = fromIO Nothing $ do
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
      clearDisableDetails = fromIO Nothing $ do
        set eFirst    [ enabled := False, WX.text := "" ]
        set eLast     [ enabled := False, WX.text := "" ]
        set ePhone    [ enabled := False, WX.text := "" ]
        set ePriority [ enabled := False, WX.text := "" ]

      trapError :: Either String () -> IO ()
      trapError x =
          case x of
              Left y -> errorDialog f "error" y
              Right _ -> return ()

      handleFocus :: Bool -> WXError ()
      -- lost focus
      handleFocus False = do
          ci <- right2CI 
          itm <- fromIO Nothing $ treeCtrlGetSelection tc
          updateNode tc itm ci
      handleFocus _ = return ()

      commitStringInput :: TextCtrl a -> Bool -> WXError ()
      commitStringInput ctrl hasFocus = do
          fromIO Nothing $ set ctrl [WX.text :~ unpad]
          handleFocus hasFocus

      commitPriorityInput :: SpinCtrl a -> Bool -> WXError ()
      commitPriorityInput ctrl hasFocus = do
          fromIO Nothing $ set ctrl [ selection :~ fromEnum . mkPriority ]
          handleFocus hasFocus

  set mFile  [ WX.text := "&File"    ]
  set iNew   [ WX.text := "&New", on command := do
                 -- TODO: What about an unsaved file?
                 varSet file defaultFile
                 new f tc defaultFile
             ]
  set iOpen  [ WX.text := "&Open...", on command := do
      name <- fileOpenDialog f True True "Open phone directory"
          fileTypesSelection "" ""
      case name of
          Just name' -> do
              putStrLn $ "Opening " ++ name'
              runErrorT ( do
                  doc <- load name'
                  populateTree tc doc
                  fromIO Nothing $ varSet file name'
                  fromIO Nothing $ set f [WX.text := title name']
                  ) >>= trapError
          Nothing -> return ()
             ]
  set iSave [ WX.text := "&Save"
            , on command := runErrorT ( do
                    doc <- tree2Doc tc
                    file' <- fromIO Nothing $ varGet file
                    let doc' = sortDoc doc
                    save file' doc'
                    M.when (doc /= doc') $ populateTree tc doc'
                ) >>= trapError
            ]
  set iSaveAs
      [ WX.text := "Save &As..."
      , on command := do
          name <- fileSaveDialog f True True "Save phone directory"
                  fileTypesSelection "" ""
          case name of
            Just name' -> runErrorT ( do
                  doc' <- tree2Doc tc
                  let doc'' = sortDoc doc'
                  save name' doc''
                  M.when (doc' /= doc'') $ populateTree tc doc''
                  fromIO Nothing $ varSet file name'
                  fromIO Nothing $ set f [WX.text := title name']
              ) >>= trapError
            Nothing -> return ()
      ]

  set iExport
      [ WX.text := "Ex&port..."
      , on command := do
          name <- fileSaveDialog f True True "Export phone directory"
                  exportTypesSelection "" ""
          case name of
            Just name' ->
              runErrorT ( do
                  doc <- tree2Doc tc
                  fromIO Nothing $ generate doc name'
              ) >>= trapError
            Nothing -> return ()
      ]

  set iQuit  [ on command := close f ]
  set iAbout [ on command := infoDialog f "About Phone Directory" aboutTxt ]

  set eFirst    [ processEnter := True
                , on command   := runErrorT (commitStringInput eFirst False) >>= trapError
                , on focus     := \x -> runErrorT (commitStringInput eFirst x) >>= trapError
                , tooltip := "Enter the contact's first name.  If the contact \
                             \only goes by a single name, enter it either \
                             \here or in the last name field."]
  set eLast     [ processEnter := True
                , on command   := runErrorT (commitStringInput eLast False) >>= trapError
                , on focus     := \x -> runErrorT (commitStringInput eLast x) >>= trapError
                , tooltip := "Enter the contact's last name.  If the contact \
                             \only goes by a single name, enter it either \
                             \here or in the first name field."]
  set ePhone    [ processEnter := True
                , on command   := runErrorT (commitStringInput ePhone False) >>= trapError
                , on focus     := \x -> runErrorT (commitStringInput ePhone x) >>= trapError
                , tooltip := "Enter thecontact's phone number." ]
  set ePriority [ on select := runErrorT (commitPriorityInput ePriority False) >>= trapError
                , on focus  := \x -> runErrorT (commitPriorityInput ePriority x) >>= trapError
                , tooltip := "Low values will sort before contacts with \
                             \higher values." ]

  set tc [ on treeEvent := onTreeEvent ]
  
  set pLeft [ layout := WX.fill $ widget tc ]
  set pRight
      [ layout := margin winPadding $ column ctrlPadding
          [ labeled "First Name:"   $ widget eFirst
          , labeled "Last Name:"    $ widget eLast
          , labeled "Phone Number:" $ widget ePhone
          , labeled "Priority:"     $ widget ePriority ]
      ]

  -- the filename has already been set
  varGet file >>= new f tc

  set f [ menuBar    := [mFile, mHelp]
        , layout     := WX.fill $ margin winPadding $ vsplit sw winPadding 200
                        (widget pLeft) (widget pRight)
        , clientSize := sz 640 480
        ]

  windowSetFocus tc

-- |Scrap and rebuild the heirarchical tree.  Once this is done, expand it and
-- select the first non-root node.
populateTree :: (Show b) => TreeCtrl a -> Document b -> WXError ()
populateTree tc doc =
    let addItem p itm = fromIO Nothing $ do
          tc' <- treeCtrlAppendItem tc p (show itm) 0 0 objectNull
          treeCtrlSetItemClientData tc tc' (return ()) itm
          return tc'
        populateOrg p org = do
          orgTc <- addItem p $ oInfo org
          mapM_ (addItem orgTc) $ oContacts org
    in do
      fromIO Nothing $ treeCtrlDeleteAllItems tc
      root <- fromIO Nothing $ treeCtrlAddRoot tc "Organizations" 0 0 objectNull
      mapM_ (populateOrg root) $ dOrganizations doc
      fromIO Nothing $ treeCtrlExpand tc root
      fromIO Nothing (treeCtrlGetNextVisible tc root >>= treeCtrlSelectItem tc)

-- |Set the supplied frame's title bar based on the supplied file.
title :: FilePath -> String
title = (++ " - Phone Directory") . takeBaseName

-- |Create a Document object based on the tree heirarchy.
tree2Doc :: TreeCtrl a -> WXError (Document (ContactInfo Name))
tree2Doc tc = do
    root <- getRootNode tc
    orgs <- fromIO Nothing $ treeCtrlWithChildren tc root $ \itm ->
        runErrorT $ do
            orgCI <- treeItem2CI tc itm
            contacts <- fromIO Nothing $ treeCtrlWithChildren tc itm $
                \itm' -> runErrorT $ treeItem2CI tc itm'
            fromEither $ Organization orgCI <$> sequence contacts
    time <- fromIO Nothing getCurrentTime
    let (year, month, day) = toGregorian $ utctDay time
        date = show month ++ "/" ++ show day ++ "/" ++ show year
    fromEither $ Document date <$> sequence orgs

getRootNode :: TreeCtrl a -> WXError TreeItem
getRootNode tc
    = fromIO (Just "Failed to retreive the root node of the tree.")
    $ treeCtrlGetRootItem tc

-- |Create a ContactInfo by pulling it from the tree heirarchy.  This is the
-- only place where 'unsafeTreeCtrlGetItemClientData' should be called.
treeItem2CI :: TreeCtrl a -> TreeItem -> WXError (ContactInfo Name)
treeItem2CI tc itm = do
    ci <- fromIO Nothing $ unsafeTreeCtrlGetItemClientData tc itm
    fromMaybe msg ci
  where
    msg = "Unable to retreive contact information from the tree heirarchy."

-- |Reset the GUI to a new file.
new
    :: Frame a     -- ^ Window with a title needing to be
    -> TreeCtrl a  -- ^ Main window's tree control.  This will be cleared.
    -> String      -- ^ Filename
    -> IO ()
new f tc fn = do
    runErrorT $ populateTree tc (mkDocument :: Document Name)
    set f [WX.text := title fn]

-- |Save the supplied document to a file.
save :: FilePath -> Document (ContactInfo Name) -> WXError ()
save fp =
  let
    msg = "Something went wrong while saving " ++ fp ++ "."
  in
    fromIO (Just msg) . writeFile fp . show . pp_value . showJSON

-- |Attempt to load a document from the supplied file.
load :: FilePath -> WXError (Document (ContactInfo Name))
load fp = do
    s <- fromIO (Just msg) $ readFile fp
    fromJSONResult $ decodeStrict s >>= readJSON
  where
    msg = "Something went wrong while loading " ++ fp ++ "."

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
updateNode tc itm ci = fromIO Nothing $ do
    -- Never update the root node.
    root <- treeCtrlGetRootItem tc
    M.when (root /= itm && treeItemIsOk itm) $ do
        treeCtrlSetItemClientData tc itm (return ()) ci
        treeCtrlSetItemText tc itm $ show ci

