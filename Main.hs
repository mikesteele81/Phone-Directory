module Main where

import Graphics.PDF
import Graphics.UI.WX as WX
import Graphics.UI.WXCore hiding (Document, fill)
import System.Console.GetOpt
import System.Environment ( getArgs )
import System.IO
import Text.JSON

import Constants
import ContactInfo
import Document
import Name
import Organization

data Mode = Generate | Edit
            deriving (Show)

data Options = Options
    { optInput :: String
    , optOutput :: String
    , optMode :: Mode
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
  withFile (optInput opts) ReadMode $ \h -> do
         input <- hGetContents h         
         case decodeStrict input >>= readJSON of
           Error s -> putStrLn $ "Error: " ++ s
           Ok doc    ->
               do
                 putStrLn "Done!"
                 case optMode opts of
                   Generate -> generate doc opts
                   Edit     -> start $ edit doc opts

generate :: Document Name -> Options -> IO ()
generate doc opts = do
  runPdf (optOutput opts) standardDocInfo (PDFRect 0 0 page_width page_height) $
         do renderDoc doc

edit :: Document Name -> Options -> IO ()
edit doc _ = do
  f  <- frame            []
  sw <- splitterWindow f []
  
  pLeft  <- panel sw []
  pRight <- panel sw []
  
  mFile  <- menuPane        []
  iNew   <- menuItem mFile  []
  ()     <- menuLine mFile
  iQuit  <- menuQuit mFile  []
  mHelp  <- menuHelp        []
  iAbout <- menuAbout mHelp []
  
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
      onTreeEvent (TreeSelChanged itm _) | treeItemIsOk itm = do
        updateRight itm
        propagateEvent
      onTreeEvent _ = propagateEvent
      
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
            set eFirst    [ enabled := False ]
            set eLast     [ enabled := False ]
            set ePhone    [ enabled := False ]
            set ePriority [ enabled := False ]

  set mFile  [ WX.text := "&File" ]
  set iNew   [ WX.text := "&Open" ]
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
      root <- treeCtrlAddRoot tc "Organizations" 0 0 objectNull
      mapM_ (populateOrg root) $ dOrganizations doc
      treeCtrlSelectItem tc root

parseOpts :: [String] -> IO Options
parseOpts argv = 
    let header = "Usage: main [OPTION...]"
    in
      case getOpt Permute options argv of
        (o,[],[]  ) -> return $ foldl (flip id) defaultOptions o
        (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
