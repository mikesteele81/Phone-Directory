module Objects
    where

import Data.List (sort)
import Text.JSON

import ContactInfo
import Name
    
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
testOrg =
    Organization
    { oInfo = ContactInfo
              { cName = SingleName "Org A"
              , cPhone = "111-111-1111"
              , cPriority = 1 }
    , oContacts =
        [ ContactInfo
          { cName = FirstLast "Brett" "Zeldman"
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