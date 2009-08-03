module Main where

import Control.Monad
import Graphics.UI.WX (start)
import System.Console.GetOpt
import System.Environment ( getArgs )
import System.IO
import Text.JSON

import Constants
import Document
import GUI
import Name
import PDF

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
  case optMode opts of
    Generate -> do
      withFile (optInput opts) ReadMode $ \h -> do
        input <- hGetContents h
        case decodeStrict input >>= readJSON :: Result (Document Name) of
          Error s -> putStrLn ("Error: " ++ s)
          Ok doc -> generate doc $ optOutput opts
    Edit -> start edit

parseOpts :: [String] -> IO Options
parseOpts argv =
    let header = "Usage: main [OPTION...]"
    in
      case getOpt Permute options argv of
        (o,[],[]  ) -> return $ foldl (flip id) defaultOptions o
        (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
