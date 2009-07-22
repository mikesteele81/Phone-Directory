{-# LANGUAGE ExistentialQuantification #-}
module Objects
    where

import Data.List (sort)
import Text.JSON

import Name
    
data ContactInfo name = ContactInfo
    { cName :: name
    , cPhone    :: String
    , cPriority :: Int
    } deriving (Eq)

instance (JSON a) => JSON (ContactInfo a) where
    readJSON (JSObject o) =
        do
          name <- valFromObj "name" o
          phone <- valFromObj "phone" o
          priority <- valFromObj "priority" o
          return $ ContactInfo name phone priority
    readJSON _ = Error "Could not parse ContactInfo JSON object."
    showJSON (ContactInfo n p pr) =
        showJSON $ toJSObject $
                     [ ("name", showJSON n )
                     , ("phone", showJSON p)
                     , ("priority", showJSON pr) ]

instance forall a. (Ord a) => Ord (ContactInfo a) where
    compare l r =                     
        -- priority descending
        case compare (cPriority r) (cPriority l) of
          -- name ascending
          EQ -> case compare (cName l) (cName r) of
                  -- phone ascending
                  EQ -> compare (cPhone l) (cPhone r)
                  x  -> x
          x  -> x
          
instance Functor ContactInfo where
    f `fmap` x = x { cName = (f . cName) x }

data LineItem = LineItem
    { liLabel :: String
    , liPhone :: String
    , liDepth :: Int
    } deriving (Show)
    
-- An organization persons are a part of
data Organization name
    = Organization
      { oInfo :: ContactInfo name
      , oContacts :: [ContactInfo name]
      } deriving (Eq)

instance (JSON a) => JSON (Organization a) where
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
                 
instance (Ord a) => Ord (Organization a) where
    compare l r = compare (oInfo l) (oInfo r)

sortOrg :: (Ord a) => Organization a -> Organization a
sortOrg o =
    o { oContacts = sort (oContacts o) }

                 
data Document name =
    Document
    { dRevised :: String
    , dOrganizations :: [Organization name] }
      
instance (JSON a) => JSON (Document a) where
    readJSON (JSObject d) =
        do revised <- valFromObj "revised" d
           organizations <- valFromObj "organizations" d
           return $ Document revised organizations
    readJSON _ = Error "Could not parse Document JSON object."
    showJSON d =
        showJSON $ toJSObject $
        [ ("revised", showJSON $ dRevised d)
        , ("organizations", showJSONs $ dOrganizations d) ]
        
sortDoc :: (Ord a) => Document a -> Document a
sortDoc d =
    d { dOrganizations = map sortOrg $ 
                         sort (dOrganizations d) }
        
testDoc :: Document Name
testDoc = Document
          { dRevised = "07/01/09"
          , dOrganizations =
            [ testOrg
            , Organization
              { oInfo = ContactInfo
                        { cName = SingleName "Beta Enterprises"
                        , cPhone = "222-222-2222"
                        , cPriority = 1 }
              , oContacts =
                [ ContactInfo
                  { cName =  FirstLast "Michael" "Steele"
                  , cPhone = "222-222-2223"
                  , cPriority = 1 } ] } ] }
                    
testOrg :: Organization Name
testOrg = Organization
          { oInfo = ContactInfo
                    { cName = SingleName "Org A"
                    , cPhone = "111-111-1111"
                    , cPriority = 1 }
          , oContacts =
              [ ContactInfo
                { cName = FirstLast "Brett" "Anderson"
                , cPhone = "111-111-1112"
                , cPriority = 1 }
              , ContactInfo
                { cName = FirstLast "Alex" "Boontidy"
                , cPhone = "111-111-1112"
                , cPriority = 1 }
              , ContactInfo
                { cName = SingleName "FAX"
                , cPhone = "111-111-1113"
                , cPriority = 0 }
              ]
          }

orgToLineItems :: (Show a) => Organization a -> [LineItem]
orgToLineItems o =
    let rest = map cIToLineItem $ oContacts o
        oi = oInfo o
    in LineItem { liLabel = show $ cName oi, liPhone = cPhone oi, liDepth = 0 } : rest

cIToLineItem :: (Show a) => ContactInfo a -> LineItem
cIToLineItem ci =
    LineItem
    { liLabel = show $ cName ci
    , liPhone = cPhone ci
    , liDepth = 1 }
