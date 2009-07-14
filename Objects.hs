module Objects
    (
    ) where


data ContactInfo = ContactInfo
    { cName :: Name
    , cPhone    :: String
    , cPriority :: Int
    } deriving (Show)

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


-- An organization persons are a part of
data Organization
    = Organization
      { oInfo :: ContactInfo
      , oContacts :: [ContactInfo]
      } deriving (Show)

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

