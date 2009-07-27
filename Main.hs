module Main where

import Graphics.PDF
import System.Console.GetOpt
import System.Environment ( getArgs )
import System.IO
import Text.JSON
import qualified Text.JSON.Pretty as JP

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
  res <- withFile (optInput opts) ReadMode grabJSON
  case res of
    Error s -> putStrLn $ "Error: " ++ s
    Ok x    ->
        do
          putStrLn "Done!"
          putStrLn $ show $ JP.pp_value x
          case optMode opts of
            Generate ->
                case (readJSON x :: Result (Document Name)) of
                  Error s -> putStrLn $ s
                  Ok x' -> runPdf (optOutput opts) standardDocInfo
                           (PDFRect 0 0 page_width page_height) $
                           do renderDoc x'
            Edit -> undefined

parseOpts :: [String] -> IO Options
parseOpts argv = 
    let header = "Usage: main [OPTION...]"
    in
      case getOpt Permute options argv of
        (o,[],[]  ) -> return $ foldl (flip id) defaultOptions o
        (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))

grabJSON :: Handle -> IO (Result JSValue)
grabJSON h = do
  hStr <- hGetContents h
  putStrLn $ hStr
  return $ decodeStrict hStr
