module Main where

import Data.Maybe (fromJust)
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
  
  pLeft  <- panel sw []
  pRight <- panel sw []
  
  mFile  <- menuPane        []
  iNew   <- menuItem mFile  []
  iSave  <- menuItem mFile  []
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
      onTreeEvent (TreeSelChanged itm' itm) | treeItemIsOk itm' = do
        updateLeft itm
        updateRight itm'
        propagateEvent
      onTreeEvent _ = propagateEvent
      
      right2CI :: IO (ContactInfo Name)
      right2CI = do
        firstName <- textCtrlGetValue eFirst
        lastName <- textCtrlGetValue eLast
        phone <- textCtrlGetValue ePhone
        priority <- textCtrlGetValue ePriority
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

  set mFile  [ WX.text := "&File" ]
  set iNew   [ WX.text := "&Open" ]
  set iSave  [ WX.text := "&Save", on command := tree2Doc tc >>= save (optInput opts) ]
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
      treeCtrlExpand tc root

-- | Create a Document object based on the tree.      
tree2Doc :: TreeCtrl a -> IO (Document Name)
tree2Doc tc = do
  root <- treeCtrlGetRootItem tc
  orgs <- treeCtrlWithChildren tc root $ \itm -> do
    orgCI <- unsafeTreeCtrlGetItemClientData tc itm >>= fromJust
    contacts <- treeCtrlWithChildren tc itm $ \itm' -> do
      unsafeTreeCtrlGetItemClientData tc itm' >>= fromJust
    return $ Organization
               { oInfo = orgCI
               , oContacts = contacts
               }
  return $ sortDoc $ Document
             { dRevised = "TODO - add date"
             , dOrganizations = orgs
             }
parseOpts :: [String] -> IO Options
parseOpts argv = 
    let header = "Usage: main [OPTION...]"
    in
      case getOpt Permute options argv of
        (o,[],[]  ) -> return $ foldl (flip id) defaultOptions o
        (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))

save :: FilePath -> Document Name -> IO ()
save file = writeFile file . show . pp_value . showJSON