{-# LANGUAGE ExistentialQuantification #-}
module ContactInfo
    ( ContactInfo (..)
    ) where

import Data.Monoid    
import Text.JSON
    
data ContactInfo name = ContactInfo
    { cName :: name
    , cPhone    :: String
    -- for the purposes of sorting.  Higher numbers sort first.
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
        compare (cPriority r) (cPriority l) `mappend`
        compare (cName l) (cName r) `mappend`
        compare (cPhone l) (cPhone r)

-- Perform an operation on the name.  Is this an abuse of Functors?          
instance Functor ContactInfo where
    f `fmap` x = x { cName = (f . cName) x }
