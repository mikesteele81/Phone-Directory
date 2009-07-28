module Main where

import Graphics.PDF
import Graphics.UI.WX as WX
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
edit _ _ = do
  f <- frame [WX.text := "Hello!"]
  quit <- button f [WX.text := "Quit", on command := close f]
  set f [layout := widget quit]

parseOpts :: [String] -> IO Options
parseOpts argv = 
    let header = "Usage: main [OPTION...]"
    in
      case getOpt Permute options argv of
        (o,[],[]  ) -> return $ foldl (flip id) defaultOptions o
        (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
