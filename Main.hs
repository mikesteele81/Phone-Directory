module Main where

import Graphics.PDF
import System.IO
import Text.JSON
--import qualified Text.JSON.Pretty as JP

import Constants
import Document
import Objects

--main1 :: IO ()
--main1 = do
--  putStrLn "Opening test.json..."
--  res <- withFile "test.json" ReadMode grabJSON
--  case res of
--    Error s -> putStrLn $ "Error: " ++ s
--    Ok x    ->
--        do
--          putStrLn "Done!"
--          putStrLn $ show $ JP.pp_value x

main :: IO ()  
main =
    runPdf "test.pdf" standardDocInfo (PDFRect 0 0 page_width page_height) $
    do
      renderDoc testDoc

--grabJSON :: Handle -> IO (Result JSValue)
--grabJSON h = do
--  hStr <- hGetContents h
--  putStrLn $ hStr
--  return $ decodeStrict hStr
