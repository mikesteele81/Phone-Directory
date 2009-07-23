module Main where

import Graphics.PDF
import System.IO
import Text.JSON
--import qualified Text.JSON.Pretty as JP

import Constants
import ContactInfo
import Name
import Objects
import Organization

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

testCI :: ContactInfo Name
testCI = ContactInfo (FirstLast "Michael" "Steele") "911" 1

main :: IO ()  
main =
    runPdf "test.pdf" standardDocInfo (PDFRect 0 0 page_width page_height) $
    do
      p <- addPage Nothing
      drawWithPage p $ do
        drawText $ text font_title title_inset title_rise title_string
        drawText $ text font_normal date_inset date_rise date_string
        drawText $ text font_normal mode_inset mode_rise mode_string
        drawCI testCI 300.0 400.0 line_item_width
        drawOrg testOrg 200.0 600.0
        beginPath (300 :+ 300)
        lineto (350 :+ 320)
        strokePath
        closePath
        drawText $ text font_normal 0.0 0.0 $ toPDFString "!"

grabJSON :: Handle -> IO (Result JSValue)
grabJSON h = do
  hStr <- hGetContents h
  putStrLn $ hStr
  return $ decodeStrict hStr