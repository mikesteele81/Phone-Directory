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

import Graphics.UI.WX (start)
import System.Console.GetOpt
import System.Environment (getArgs)

import GUI

data Action = MakeNew | Open

data Options = Options
    { file :: String
    , action :: Action }

defaultOptions :: Options
defaultOptions = Options "" MakeNew

options :: [OptDescr (Options -> Options)]
options =
    [ Option "o" ["open"] (ReqArg (\f opts -> opts {file = f, action = Open})
        "FILE") "open FILE"
    ]

main :: IO ()
main = do
    opts <- getArgs >>= parseOpts
    case action opts of
        MakeNew -> start mainWindow
        Open    -> start mainWindow

parseOpts :: [String] -> IO Options
parseOpts argv =
    case getOpt Permute options argv of
        (o, [], []) -> return $ foldl (flip id) defaultOptions o
        (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where
    header = "Usage: main [OPTION...]"
