module Main where

import Graphics.PDF
import Graphics.UI.WX as WX
import Graphics.UI.WXCore hiding (Document, fill)
import System.Console.GetOpt
import System.Environment ( getArgs )
import System.IO
import Text.JSON

import Constants
import Document
import Name

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
  
  tc <- treeCtrl pLeft []
  
  set mFile  [ WX.text := "&File" ]
  set iNew   [ WX.text := "&Open" ]
  set iQuit  [ on command := close f ]
  set iAbout [ on command := infoDialog f "About Phone Directory" "test" ]
  
  tFirst    <- staticText pRight [ WX.text := "First Name:"   ]
  tLast     <- staticText pRight [ WX.text := "Last Name:"    ]
  tPhone    <- staticText pRight [ WX.text := "Phone Number:" ]
  tPriority <- staticText pRight [ WX.text := "Priority:"     ]
  
  set tc [ ] --on treeEvent := onTreeEvent tc ]
  
  set pLeft [ layout := WX.fill $ widget tc ]
  set pRight [ layout := margin 6 $ column 5
                          [ widget tFirst, widget tLast
                          , widget tPhone, widget tPriority
                          ]
             ]

  populateTree tc doc

  set f [ WX.text    := "Phone Directory"
        , menuBar    := [mFile, mHelp]
        , layout     := WX.fill $ vsplit sw 5 200 (widget pLeft) (widget pRight)
        , clientSize := sz 640 480
        ]

populateTree :: TreeCtrl a -> Document Name -> IO ()
populateTree tc doc = do
  root <- treeCtrlAddRoot tc "Organizations" 0 0 objectNull
  treeCtrlSetItemClientData tc root (return ()) doc
  mapM_ (\x -> treeCtrlAppendItem tc root (show x) 0 0 objectNull) $ dOrganizations doc
  treeCtrlSelectItem tc root

parseOpts :: [String] -> IO Options
parseOpts argv = 
    let header = "Usage: main [OPTION...]"
    in
      case getOpt Permute options argv of
        (o,[],[]  ) -> return $ foldl (flip id) defaultOptions o
        (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))

--onTreeEvent :: TreeCtrl () -> EventTree -> IO ()
--onTreeEvent tc (TreeItemExpanding itm veto) | treeItemIsOk itm = do
--  wxcBeginBusyCursor
--  children <- treeCtrlGetChildren tc itm
--  mapM_ visualise children
--  wxcEndBusyCursor
--  propagateEvent
--onTreeEvent tc (TreeSelChanged itm olditem) | treeItemIsOk itm = do
--  wxcBeginBusyCursor
--  selectRight item
--  wxcEndBusyCursor
--  propagateEvent
--onTreeEvent _ _ = propagateEvent

--visualise :: TreeCtrl () -> TreeItem -> IO ()
--visualise tc item = do
--  c <- treeCtrlItemHasChildren tc item
--  when (c == 0) $ giveBirth item
--  updateThickness item
--  updateImages item