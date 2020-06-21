module AssocListTests where

import Test.QuickCheck
import Data.Maybe (fromJust)
import Data.List (sort)
import qualified AssocList as AL
import qualified Data.Map as DM

prop_invarEmpty :: Bool
prop_invarEmpty = AL.invar (AL.empty :: AL.Map Int Int) -- need to explicitly state type variables for this test

prop_invarInsert :: Eq k => k -> v -> AL.Map k v -> Property
prop_invarInsert k v m = AL.invar m ==> AL.invar (AL.insert k v m)

prop_invarDelete :: Eq k => k -> AL.Map k v -> Property
prop_invarDelete k m = AL.invar m ==> AL.invar (AL.delete k m)

hom :: Ord k => AL.Map k v -> DM.Map k v
hom m = foldr (\k -> DM.insert k $ fromJust $ AL.lookup k m) DM.empty $ AL.keys m

prop_simEmpty :: Bool
prop_simEmpty = hom (AL.empty :: AL.Map Integer String) == DM.empty

prop_simInsert :: (Ord k, Eq v) => k -> v -> AL.Map k v -> Property
prop_simInsert k v m = AL.invar m ==> hom (AL.insert k v m) == DM.insert k v (hom m)

prop_simLookup :: (Ord k, Eq v) => k -> AL.Map k v -> Property
prop_simLookup k m = AL.invar m ==> AL.lookup k m == DM.lookup k (hom m)

prop_simDelete :: (Ord k, Eq v) => k -> AL.Map k v -> Property
prop_simDelete k m = AL.invar m ==> hom (AL.delete k m) == DM.delete k (hom m)

prop_simKeys :: Ord k => AL.Map k v -> Property
prop_simKeys m = AL.invar m ==> sort (AL.keys m) == DM.keys (hom m)
