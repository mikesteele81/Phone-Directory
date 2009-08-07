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

module Main where

import Control.Monad
import Graphics.UI.WX (start)
import System.Console.GetOpt
import System.Environment ( getArgs )
import System.IO
import Text.JSON

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
                 , optMode = Edit
                 }

options :: [OptDescr (Options -> Options)]
options =
    [ Option "o" ["output"]
      (ReqArg (\f opts -> opts { optOutput = f }) "FILE") "output FILE"
    , Option "c" []
      (ReqArg (\f opts -> opts { optInput  = f }) "FILE") "input FILE"
    , Option "g" ["generate"]
      (NoArg (\ opts -> opts { optMode = Generate })) "generate"
    , Option "e" ["edit"]
      (NoArg (\ opts -> opts { optMode = Edit })) "edit"
    ]

main :: IO ()
main = do
  opts <- getArgs >>= parseOpts
  print opts
  putStrLn $ "Opening " ++ optInput opts ++ "..."
  case optMode opts of
    Generate -> withFile (optInput opts) ReadMode $ \h -> do
                  input <- hGetContents h
                  case decodeStrict input >>= readJSON :: Result (Document Name) of
                    Error s -> putStrLn ("Error: " ++ s)
                    Ok doc -> generate doc $ optOutput opts
    Edit     -> start edit

parseOpts :: [String] -> IO Options
parseOpts argv =
    let header = "Usage: main [OPTION...]"
    in
      case getOpt Permute options argv of
        (o,[],[]  ) -> return $ foldl (flip id) defaultOptions o
        (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
