module Objects
    where

import Control.Applicative
import Data.List (sort)
import Text.JSON
    
data (Show name, Ord name) => ContactInfo name = ContactInfo
    { cName :: name
    , cPhone    :: String
    , cPriority :: Int
    } deriving (Eq, Show)

instance (JSON a, Show a, Ord a) => JSON (ContactInfo a) where
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

instance (Ord a, Show a) => Ord (ContactInfo a) where
    compare l r =                     
        -- priority descending
        case compare (cPriority r) (cPriority l) of
          -- name ascending
          EQ -> case compare (cName l) (cName r) of
                  -- phone ascending
                  EQ -> compare (cPhone l) (cPhone r)
                  x  -> x
          x  -> x
                     
data LineItem = LineItem
    { liLabel :: String
    , liPhone :: String
    , liDepth :: Int
    } deriving (Show)
    
data Name = FirstLast { nFirst :: String
                      , nLast :: String }
          | SingleName { nSingleName :: String
          } deriving (Eq)
          
newtype FirstSortedName = FirstSortedName { unFirstSortedName :: Name }
    deriving (Eq)

newtype LastSortedName = LastSortedName { unLastSortedName :: Name }
    deriving (Eq)

instance Show FirstSortedName where
    show (FirstSortedName n) =
        case n of
          FirstLast f l -> f ++ " " ++ l
          SingleName sn -> sn
    
instance Show LastSortedName where
    show (LastSortedName n) =
        case n of
          FirstLast f l -> l ++ ", " ++ f
          SingleName sn -> sn
          
class ShowForSorting a where
    showForSorting :: a -> String
    
instance ShowForSorting FirstSortedName where
    showForSorting (FirstSortedName n) =
        case n of
          FirstLast f l -> f ++ l
          SingleName sn -> sn
          
instance ShowForSorting LastSortedName where
    showForSorting (LastSortedName n) =
        case n of
        FirstLast f l -> l ++ f
        SingleName sn -> sn

instance Ord FirstSortedName where
    compare l r = compare (showForSorting l) (showForSorting r)
instance Ord LastSortedName where
    compare l r = compare (showForSorting l) (showForSorting r)
    
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
                   
instance JSON FirstSortedName where
    readJSON n = readJSON n >>= (return . FirstSortedName)
    showJSON (FirstSortedName n) = showJSON n
    
instance JSON LastSortedName where
    readJSON n = readJSON n >>= (return . LastSortedName)
    showJSON (LastSortedName n) = showJSON n


-- An organization persons are a part of
data (Show name) => Organization name
    = Organization
      { oInfo :: ContactInfo name
      , oContacts :: [ContactInfo name]
      } deriving (Eq, Show)

instance (JSON a, Ord a, Show a) => JSON (Organization a) where
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
                 
instance (Ord a, Show a) => Ord (Organization a) where
    compare l r = compare (oInfo l) (oInfo r)

sortOrg :: (Ord a, Show a) => Organization a -> Organization a
sortOrg o =
    o { oContacts = sort (oContacts o) }

                 
data (Show name) => Document name
    = Document
      { dRevised :: String
      , dOrganizations :: [Organization name]
      } deriving (Show)
      
instance (JSON a, Ord a, Show a) => JSON (Document a) where
    readJSON (JSObject d) =
        do revised <- valFromObj "revised" d
           organizations <- valFromObj "organizations" d
           return $ Document revised organizations
    readJSON _ = Error "Could not parse Document JSON object."
    showJSON d =
        showJSON $ toJSObject $
        [ ("revised", showJSON $ dRevised d)
        , ("organizations", showJSONs $ dOrganizations d) ]
        
sortDoc :: (Ord a, Show a) => Document a -> Document a
sortDoc d =
    d { dOrganizations = map sortOrg $ 
                         sort (dOrganizations d) }
        
testDoc :: Document LastSortedName
testDoc = Document
          { dRevised = "07/01/09"
          , dOrganizations =
            [ testOrg
            , Organization
              { oInfo = ContactInfo
                        { cName = LastSortedName $ 
                                  SingleName "Beta Enterprises"
                        , cPhone = "222-222-2222"
                        , cPriority = 1 }
              , oContacts =
                [ ContactInfo
                  { cName = LastSortedName $
                            FirstLast "Michael" "Steele"
                  , cPhone = "222-222-2223"
                  , cPriority = 1 } ] } ] }
                    
testOrg :: Organization LastSortedName
testOrg = Organization
          { oInfo = ContactInfo
                    { cName = LastSortedName $
                              SingleName "Org A"
                    , cPhone = "111-111-1111"
                    , cPriority = 1 }
          , oContacts =
              [ ContactInfo
                { cName = LastSortedName $
                          FirstLast "Brett" "Anderson"
                , cPhone = "111-111-1112"
                , cPriority = 1 }
              , ContactInfo
                { cName = LastSortedName $
                          FirstLast "Alex" "Boontidy"
                , cPhone = "111-111-1112"
                , cPriority = 1 }
              , ContactInfo
                { cName = LastSortedName $
                          SingleName "FAX"
                , cPhone = "111-111-1113"
                , cPriority = 0 }
              ]
          }

orgToLineItems :: (Ord a, Show a) => Organization a -> [LineItem]
orgToLineItems o =
    let rest = map cIToLineItem $ oContacts o
        oi = oInfo o
    in LineItem { liLabel = show $ cName oi, liPhone = cPhone oi, liDepth = 0 } : rest

cIToLineItem :: (Ord a, Show a) => ContactInfo a -> LineItem
cIToLineItem ci =
    LineItem
    { liLabel = show $ cName ci
    , liPhone = cPhone ci
    , liDepth = 1 }
