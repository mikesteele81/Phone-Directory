module PageProperties where

import Control.Applicative
import Control.Monad.Error
import Text.JSON
import Text.JSON.Pretty

import UnitConversion

data Layout = Portrait | Landscape
    deriving (Read, Show)

instance JSON Layout where
    readJSON (JSString str) = return . read . fromJSString $ str
    readJSON v = Error $ "Expected JSString, but " ++ (show . pp_value) v
        ++ " found while parsing a contact information."
    showJSON l = showJSON . show $ l

data PageProperties = PageProperties
    { layout       :: Layout
    , leftMargin   :: Inches
    , rightMargin  :: Inches
    , topMargin    :: Inches
    , bottomMargin :: Inches }
    deriving (Show)

instance JSON PageProperties where
    readJSON (JSObject o) = do
        l  <- (valFromObj "layout"       o <|> return Portrait)
        lm <- (valFromObj "leftMargin"   o <|> return halfInch)
        rm <- (valFromObj "rightMargin"  o <|> return halfInch)
        tm <- (valFromObj "topMargin"    o <|> return halfInch)
        bm <- (valFromObj "bottomMargin" o <|> return halfInch)
        return $ PageProperties l lm rm tm bm
    readJSON v = Error $ "Expected JSObject, but " ++ (show . pp_value) v
        ++ " found while parsing a contact information."
    showJSON pp = makeObj
        [ ("layout"      , showJSON . layout       $ pp)
        , ("leftMargin"  , showJSON . leftMargin   $ pp)
        , ("rightMargin" , showJSON . rightMargin  $ pp)
        , ("topMargin"   , showJSON . topMargin    $ pp)
        , ("bottomMargin", showJSON . bottomMargin $ pp) ]

mkPageProperties :: PageProperties
mkPageProperties = PageProperties
    { layout       = Portrait
    , leftMargin   = halfInch
    , rightMargin  = halfInch
    , topMargin    = halfInch
    , bottomMargin = halfInch }

halfInch :: Inches
halfInch = Inches 0.5