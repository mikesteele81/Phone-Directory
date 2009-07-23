module Objects
    where

import Data.List (sort)
import Text.JSON

import ContactInfo
import Name
import Organization
    
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