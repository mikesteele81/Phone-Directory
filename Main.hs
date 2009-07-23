module Main where

import Graphics.PDF
import System.IO
import Text.JSON
import qualified Text.JSON.Pretty as JP

import Constants
import Document
import Name

main :: IO ()
main = do
  putStrLn "Opening test.json..."
  res <- withFile "test.json" ReadMode grabJSON
  case res of
    Error s -> putStrLn $ "Error: " ++ s
    Ok x    ->
        do
          putStrLn "Done!"
          putStrLn $ show $ JP.pp_value x
          case (readJSON x :: Result (Document Name)) of
            Error s -> putStrLn $ s
            Ok x' -> runPdf "test.pdf" standardDocInfo
                    (PDFRect 0 0 page_width page_height) $
                    do renderDoc x'

grabJSON :: Handle -> IO (Result JSValue)
grabJSON h = do
  hStr <- hGetContents h
  putStrLn $ hStr
  return $ decodeStrict hStr
