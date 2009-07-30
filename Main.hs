module Main where

import Control.Monad
import Graphics.PDF
import Graphics.UI.WX as WX
import Graphics.UI.WXCore hiding (Document, fill)
import System.Console.GetOpt
import System.Environment ( getArgs )
import System.IO
import Text.JSON
import Text.JSON.Pretty

import Constants
import ContactInfo
import Document
import GUI
import Name
import Organization

data AppMode = Generate | Edit
            deriving (Show)

data Options = Options
    { optInput :: String
    , optOutput :: String
    , optMode :: AppMode
    } deriving Show

defaultOptions :: Options
defaultOptions = Options
                 { optInput = "test.json"
                 , optOutput = "test.pdf"
                 , optMode = Generate
                 }
                 
options :: [OptDescr (Options -> Options)]
options =
    [ Option ['o'] ["output"]
      (ReqArg (\f opts -> opts { optOutput = f }) "FILE") "output FILE"
    , Option ['c'] []
      (ReqArg (\f opts -> opts { optInput  = f }) "FILE") "input FILE"
    , Option ['g'] ["generate"]
      (NoArg (\ opts -> opts { optMode = Generate })) "generate"
    , Option ['e'] ["edit"]
      (NoArg (\ opts -> opts { optMode = Edit })) "edit"
    ]
    
main :: IO ()
main = do
  opts <- getArgs >>= parseOpts
  putStrLn $ show opts
  putStrLn $ "Opening " ++ optInput opts ++ "..."
  doc <- withFile (optInput opts) ReadMode $ \h -> do
    input <- hGetContents h
    case decodeStrict input >>= readJSON of
      Error s -> putStrLn ("Error: " ++ s) >> return Nothing
      Ok doc -> return $ Just doc
  case doc of
    Nothing -> return ()
    Just doc' -> case optMode opts of
                   Generate -> generate doc' opts
                   Edit     -> start $ edit doc' opts

generate :: Document Name -> Options -> IO ()
generate doc opts = do
  runPdf (optOutput opts) standardDocInfo (PDFRect 0 0 page_width page_height) $
         do renderDoc doc

edit :: Document Name -> Options -> IO ()
edit doc opts = do
  f  <- frame            []
  sw <- splitterWindow f []
  
  pLeft   <- panel    sw     []
  pRight  <- panel    sw     []
  
  mFile   <- menuPane        []
  iNew    <- menuItem mFile  []
  iOpen   <- menuItem mFile  []
  iSave   <- menuItem mFile  []
  iSaveAs <- menuItem mFile  []
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
      onTreeEvent :: EventTree -> IO ()
      onTreeEvent (TreeSelChanging itm' _ veto)
          | treeItemIsOk itm' = do
                  root <- treeCtrlGetRootItem tc
                  Control.Monad.when (root == itm') veto
          | otherwise = veto
      onTreeEvent (TreeSelChanged itm' itm) | treeItemIsOk itm' = do
        -- The root should never be updated
        root <- treeCtrlGetRootItem tc
        unless (root == itm) $ updateLeft itm
        
        updateRight itm'
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
          KeyDelete -> do
            unless (root == itm) $ treeCtrlDelete tc itm
          _ -> return ()
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
        return $ ContactInfo
                   { cName     = name
                   , cPhone    = phone
                   , cPriority = read priority
                   }

      -- | update left side to match what's entered on right
      updateLeft itm = do
        ci <- right2CI
        treeCtrlSetItemClientData tc itm (return ()) ci
        treeCtrlSetItemText tc itm $ show ci

      -- | refresh right side to match left selection
      updateRight :: TreeItem -> IO ()
      updateRight itm = do
        -- a ContactInfo was placed in every node save the root
        ci <- unsafeTreeCtrlGetItemClientData tc itm
        case ci of
          Just ci' -> do
            case cName ci' of
              FirstLast first l -> do
                set eFirst [ enabled := True, WX.text := first ]
                set eLast  [ enabled := True, WX.text := l     ]
              SingleName n -> do
                set eFirst [ enabled := True, WX.text := n  ]
                set eLast  [ enabled := True, WX.text := "" ]
            set ePhone    [ enabled := True, WX.text := cPhone ci' ]
            set ePriority [ enabled := True, WX.text := show $ cPriority ci' ]
          Nothing -> do
            set eFirst    [ enabled := False, WX.text := "" ]
            set eLast     [ enabled := False, WX.text := "" ]
            set ePhone    [ enabled := False, WX.text := "" ]
            set ePriority [ enabled := False, WX.text := "" ]

  set mFile  [ WX.text := "&File"    ]
  set iNew   [ WX.text := "&New"     ]
  set iOpen  [ WX.text := "&Open...", on command := do
                 name <- fileOpenDialog f True True "Open phone directory"
                         [("JSON", ["*.json"])] "" ""
                 case name of
                   Just name' -> do
                     doc' <- load name'
                     case doc' of
                       Just doc'' -> do
                                   putStrLn $ "Opening " ++ name'
                                   populateTree tc doc''
                       Nothing -> return ()
                   Nothing -> return ()
             ]
  set iSave  [ WX.text := "&Save", on command := do
                 doc' <- tree2Doc tc
                 case doc' of
                   Just doc'' -> save (optInput opts) doc''
                   Nothing -> putStrLn "bad doc" >> return ()
             ]
  set iSaveAs  [ WX.text := "Save &As...", on command := do
                   name <- fileSaveDialog f True True "Save directory"
                           file_types_selection "" ""
                   case name of
                     Just name' -> do
                       doc' <- tree2Doc tc
                       case doc' of
                         Just doc'' -> save name' doc''
                         Nothing -> return ()
                     Nothing -> return ()
               ]
  set iQuit  [ on command := close f ]
  set iAbout [ on command := infoDialog f "About Phone Directory" "test" ]
  
  set tc [ on treeEvent := onTreeEvent ]
  
  set pLeft [ layout := WX.fill $ widget tc ]
  set pRight [ layout := margin 6 $ column 5
                          [ widget tFirst, widget eFirst
                          , widget tLast, widget eLast
                          , widget tPhone, widget ePhone
                          , widget tPriority, widget ePriority
                          ]
             ]

  populateTree tc doc

  set f [ WX.text    := "Phone Directory"
        , menuBar    := [mFile, mHelp]
        , layout     := WX.fill $ vsplit sw 5 200 (widget pLeft) (widget pRight)
        , clientSize := sz 640 480
        ]

parseOpts :: [String] -> IO Options
parseOpts argv = 
    let header = "Usage: main [OPTION...]"
    in
      case getOpt Permute options argv of
        (o,[],[]  ) -> return $ foldl (flip id) defaultOptions o
        (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))

save :: FilePath -> Document Name -> IO ()
save file = writeFile file . show . pp_value . showJSON

load :: FilePath -> IO (Maybe (Document Name))
load fp = do
  withFile fp ReadMode $ \h -> do
            input <- hGetContents h
            case decodeStrict input >>= readJSON of
              Error s -> putStrLn ("Error: " ++ s) >> return Nothing
              Ok doc -> return $ Just doc
