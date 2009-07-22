module Name
    ( Name (FirstLast, SingleName)
    , FirstSortedName (FirstSortedName, unFirstSortedName)
    ) where

import Control.Applicative    
import Text.JSON
    
data Name = FirstLast String String
          | SingleName String
          deriving (Eq)
          
newtype FirstSortedName = FirstSortedName { unFirstSortedName :: Name }
    deriving (Eq)

instance Show FirstSortedName where
    show (FirstSortedName n) =
        case n of
          FirstLast f l -> f ++ " " ++ l
          SingleName sn -> sn
    
instance Show Name where
    show n =
        case n of
          FirstLast f l -> l ++ ", " ++ f
          SingleName sn -> sn
          
class ShowForSorting a where
    showForSorting :: a -> String
    
instance ShowForSorting FirstSortedName where
    showForSorting (FirstSortedName n) =
        case n of
          FirstLast f l -> f ++ l
          _             -> showForSorting n
          
instance ShowForSorting Name where
    showForSorting n =
        case n of
          FirstLast f l -> l ++ f
          SingleName sn -> sn

instance Ord Name where
    compare l r = compare (showForSorting l) (showForSorting r)

instance Ord FirstSortedName where
    compare l r = compare (showForSorting l) (showForSorting r)
    
instance JSON Name where
    readJSON (JSObject o) =
        FirstLast <$> valFromObj "first" o <*> valFromObj "last" o
        <|> SingleName <$> valFromObj "name" o
    readJSON _ = Error "boo!"
    showJSON n = showJSON $ toJSObject $
                 case n of
                   FirstLast f l -> [ ("first", showJSON f)
                                    , ("last" , showJSON l)]
                   SingleName sn -> [ ("name" , showJSON sn)]
                   
instance JSON FirstSortedName where
    readJSON n = FirstSortedName <$> readJSON n
    showJSON (FirstSortedName n) = showJSON n
    