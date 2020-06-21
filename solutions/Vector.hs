module Vector (Vector, newVector, size, capacity, resize, set, get) where

import Data.Maybe
import Test.QuickCheck hiding (resize)

newtype Vector a = Vector [Maybe a]
  deriving (Show, Eq)

newVector :: Int -> Vector a
newVector n = Vector (replicate n Nothing)

size :: Vector a -> Int
size (Vector v) = length v

capacity :: Vector a -> Int
capacity (Vector v) = length $ filter isNothing v

resize :: Vector a -> Int -> Vector a
resize v@(Vector l) n =
  if n > size v then Vector $ l ++ replicate (n - (size v)) Nothing
  else if n < size v then Vector $ take n l
  else v

set :: Vector a -> a -> Int-> Maybe (Vector a)
set v@(Vector l) x i =
  if i < 0 || i >= size v
  then Nothing
  else Just $ Vector $ take i l ++ [Just x] ++ drop (i + 1) l

get :: Vector a -> Int -> Maybe a
get v@(Vector l) i =
  if i < 0 || i >= size v
  then Nothing
  else l !! i
