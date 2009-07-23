module Objects
    where

import ContactInfo
import Document
import Name
import Organization
    
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
                [ testCI ] } ] }
                    
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
    
testCI :: ContactInfo Name
testCI = ContactInfo (FirstLast "Michael" "Steele") "911" 1

