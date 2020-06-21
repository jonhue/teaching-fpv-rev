module AssocList (Map, empty, insert, lookup, delete, keys, invar) where

import Prelude hiding (lookup)
import Data.List (nub)
import Test.QuickCheck
import Data.Maybe (isNothing)

newtype Map k v = Map [(k,v)]
  deriving (Show)

empty :: Map k v
empty = Map []

insert :: Eq k => k -> v -> Map k v -> Map k v
insert k v (Map al) = Map $ ins al
  where
    ins [] = [(k,v)]
    ins ((k',v') : m)
        | k == k' = (k,v) : m
        | otherwise = (k',v') : ins m

lookup :: Eq k => k -> Map k v -> Maybe v
lookup k (Map al) = lo al
  where
    lo [] = Nothing
    lo ((k',v') : m)
        | k == k' = Just v'
        | otherwise = lo m

delete :: Eq k => k -> Map k v -> Map k v
delete k (Map al) = Map $ del al
  where
    del [] = []
    del ((k',v') : m)
        | k == k' = m
        | otherwise = (k',v') : del m

keys :: Map k v -> [k]
keys (Map al) = map fst al

invar :: Eq k => Map k v -> Bool
invar (Map []) = True
invar (Map ((k,_):xs)) = isNothing (lookup k $ Map xs) && invar (Map xs)

instance (Eq k, Arbitrary k, Arbitrary v) => Arbitrary (Map k v) where
  arbitrary = do k <- arbitrary
                 v <- arbitrary
                 return $ Map $ zip (nub k) v
