module Objects
    where

import Control.Applicative
import Data.List (sortBy)
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
    readJSON _ = Error "Could not parse ContactInfo JSON object."
    showJSON (ContactInfo n p pr) =
        showJSON $ toJSObject $
                     [ ("name", showJSON n )
                     , ("phone", showJSON p)
                     , ("priority", showJSON pr) ]
                     
compareCIBy :: NamePreference -> ContactInfo -> ContactInfo -> Ordering
compareCIBy p l r =
    -- priority descending
    case compare (cPriority r) (cPriority l) of
      -- name ascending
      EQ -> case compareNameBy p (cName l) (cName r) of
              -- phone ascending
              EQ -> compare (cPhone l) (cPhone r)
              x  -> x
      x  -> x
                     
data LineItem = LineItem
    { liLabel :: String
    , liPhone :: String
    , liDepth :: Int
    } deriving (Show)
    
-- Used as an argument for functions that depend differently depending on
-- whether the first or last name is more interesting.
data NamePreference = FirstPreferred
                    | LastPreferred
                    -- just for debugging
                    deriving (Show)

data Name = FirstLast { nFirst :: String
                      , nLast :: String }
          | SingleName { nSingleName :: String }

-- used for debugging.          
instance Show Name where
    show = showName LastPreferred
          
showName :: NamePreference -> Name -> String
showName _ (SingleName n) = n
showName p (FirstLast f l) =
    case p of
      FirstPreferred -> f ++ " " ++ l
      LastPreferred  -> l ++ ", " ++ f

          
compareNameBy :: NamePreference -> Name -> Name -> Ordering
compareNameBy p l r =
    let concatName :: Name -> String
        concatName (SingleName n) = n
        concatName (FirstLast fn ln) =
            case p of
              FirstPreferred -> fn ++ ln
              LastPreferred  -> ln ++ fn
    in compare (concatName l) (concatName r)

instance JSON Name where
    readJSON (JSObject o) =
        let tryFL =
                do
                  first <- valFromObj "first" o
                  sirname <- valFromObj "last" o
                  return $ FirstLast first sirname
            trySN =
                do
                  name <- valFromObj "name" o
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
                 
compareOrgBy :: NamePreference -> Organization -> Organization -> Ordering
compareOrgBy p l r = compareCIBy p (oInfo l) (oInfo r)

sortOrg :: NamePreference -> Organization -> Organization
sortOrg p o =
    o { oContacts = sortBy (compareCIBy p) (oContacts o) }

                 
data Document
    = Document
      { dRevised :: String
      , dOrganizations :: [Organization]
      } deriving (Show)
      
instance JSON Document where
    readJSON (JSObject d) =
        do revised <- valFromObj "revised" d
           organizations <- valFromObj "organizations" d
           return $ Document revised organizations
    readJSON _ = Error "Could not parse Document JSON object."
    showJSON d =
        showJSON $ toJSObject $
        [ ("revised", showJSON $ dRevised d)
        , ("organizations", showJSONs $ dOrganizations d) ]
        
sortDoc :: NamePreference -> Document -> Document
sortDoc p d =
    d { dOrganizations = map (sortOrg p) $ 
                         sortBy (compareOrgBy p) (dOrganizations d) }
        
testDoc :: Document
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
                  { cName = FirstLast "Michael" "Steele"
                  , cPhone = "222-222-2223"
                  , cPriority = 1 } ] } ] }
                    
testOrg :: Organization
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
