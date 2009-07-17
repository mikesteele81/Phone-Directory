module Objects
    where

import Control.Applicative
import Text.JSON
    
data ContactInfo = ContactInfo
    { cName :: Name
    , cPhone    :: String
    , cPriority :: Int
    } deriving (Show)

instance JSON ContactInfo where
    readJSON (JSObject o) =
        do
          name <- valFromObj "name" o
          phone <- valFromObj "phone" o
          priority <- valFromObj "priority" o
          return $ ContactInfo name phone priority
    readJSON _ = Error "boo!"
    showJSON (ContactInfo n p pr) =
        showJSON $ toJSObject $
                     [ ("name", showJSON n )
                     , ("phone", showJSON p)
                     , ("priority", showJSON pr) ]

data LineItem = LineItem
    { liLabel :: String
    , liPhone :: String
    , liDepth :: Int
    } deriving (Show)

data Name = FirstLast { nFirst :: String
                      , nLast :: String }
          | SingleName { nSingleName :: String }

-- Temporary definition
instance Show Name where
    show (FirstLast f l) = l ++ ", " ++ f
    show (SingleName n)  = n 

instance JSON Name where
    readJSON (JSObject o) =
        let tryFL =
                do
                  first <- valFromObj "first" o
                  sirname <- valFromObj "last" o
                  return $ FirstLast first sirname
            trySN =
                do
                  name <- valFromObj "name" o >>= readJSON
                  return $ SingleName name
        in do tryFL <|> trySN
    readJSON _ = Error "boo!"
    showJSON n = showJSON $ toJSObject $
                 case n of
                   FirstLast f l -> [ ("first", showJSON f)
                                    , ("last" , showJSON l)]
                   SingleName sn -> [ ("name" , showJSON sn)] 

-- An organization persons are a part of
data Organization
    = Organization
      { oInfo :: ContactInfo
      , oContacts :: [ContactInfo]
      } deriving (Show)

instance JSON Organization where
    readJSON (JSObject o) =
        do
          info <- valFromObj "info" o
          contacts <- valFromObj "contacts" o
          return $ Organization info contacts
    readJSON _ = Error "Could not parse Organization JSON object."
    showJSON o =
        showJSON $ toJSObject $
                 [ ("info", showJSON $ oInfo o)
                 , ("contacts", showJSONs $ oContacts o)]

testOrg :: Organization
testOrg = Organization
          { oInfo = ContactInfo
                    { cName = SingleName "Org A"
                    , cPhone = "111-111-1111"
                    , cPriority = 0 }
          , oContacts =
              [ ContactInfo
                { cName = FirstLast "Albert" "Anderson"
                , cPhone = "111-111-1112"
                , cPriority = 1 }
              , ContactInfo
                { cName = SingleName "FAX"
                , cPhone = "111-111-1113"
                , cPriority = 0 }
              ]
          }

orgToLineItems :: Organization -> [LineItem]
orgToLineItems o =
    let rest = map cIToLineItem $ oContacts o
        oi = oInfo o
    in LineItem { liLabel = show $ cName oi, liPhone = cPhone oi, liDepth = 0 } : rest

cIToLineItem :: ContactInfo -> LineItem
cIToLineItem ci =
    LineItem
    { liLabel = show $ cName ci
    , liPhone = cPhone ci
    , liDepth = 1 }
