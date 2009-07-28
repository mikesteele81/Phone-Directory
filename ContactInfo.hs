{-# LANGUAGE ExistentialQuantification #-}
module ContactInfo
    ( ContactInfo (..)
    ) where

import Control.Applicative
import Data.Monoid
import Text.JSON

data ContactInfo a = ContactInfo
    { cName :: a
    , cPhone    :: String
    -- for the purposes of sorting.  Higher numbers sort first.
    , cPriority :: Int
    } deriving (Eq)
    
instance (JSON a) => JSON (ContactInfo a) where
    readJSON (JSObject o) =
        ContactInfo <$> valFromObj "name" o <*> valFromObj "phone" o
        <*> valFromObj "priority" o
    readJSON _ =
        Error "Could not parse ContactInfo JSON object."
    showJSON (ContactInfo n p pr) =
       makeObj [ ("name", showJSON n), ("phone", showJSON p)
               , ("priority", showJSON pr) ]

instance forall a. (Ord a) => Ord (ContactInfo a) where
    compare l r =                     
        -- priority descending
        compare (cPriority r) (cPriority l) `mappend`
        compare (cName l) (cName r) `mappend`
        compare (cPhone l) (cPhone r)

instance forall a. (Show a) => Show (ContactInfo a) where
    show = show . cName

-- Perform an operation on the name.  Is this an abuse of Functors?
instance Functor ContactInfo where
    f `fmap` x = x { cName = (f . cName) x }
