module ProblemSet_1 where

import Test.QuickCheck


{-1.1.1
  Behavior of `mystery`: ...
-}

{-1.1.2-}
threeDifferent :: Integer -> Integer -> Integer -> Bool
threeDifferent = undefined

{-1.2.1-}
pow :: Integer -> Integer -> Integer
pow = undefined

{-1.2.2-}
ascending :: Ord a => [a] -> bool
ascending = undefined

{-1.2.3-}
zip' :: [a] -> [b] -> [(a,b)]
zip' = undefined

{-1.2.4-}
insertionSort :: Ord a => [a] -> [a]
insertionSort = undefined

{-1.2.5
  (a)
  (b)
  (c)
-}

{-1.2.6-}
fac :: Int -> Int
fac = undefined

{-1.2.7-}
regions :: Integer -> Integer
regions = undefined

{-1.2.8-}
remdups ::  Eq a => [a] -> [a]
remdups = undefined

{-1.3.1-}
concat' :: [[a]] -> [a]
concat' = undefined

{-1.3.2-}
primes :: Int -> [Int]
primes = undefined

{-1.3.3-}
enumFromThenTo :: Integer -> Integer -> Integer -> [Integer]
enumFromThenTo = undefined

enumFromThen :: Integer -> Integer -> [Integer]
enumFromThen = undefined

{-1.4.1-}
prop_zip' :: [Int] -> [Int] -> Bool
prop_zip' = undefined

prop_concat' :: [[Int]] -> Bool
prop_concat' = undefined

{-1.4.2-}
occs :: Eq a => [a] -> [(a, Int)]
occs xs = aux xs []
  where
    aux :: Eq a => [a] -> [(a, Int)] -> [(a, Int)]
    aux [] ys = ys
    aux (x:xs) ys
      | x `elem` map fst ys = aux xs $ map (inc x) ys
      | otherwise           = aux xs $ (x, 1) : ys
    inc :: Eq a => a -> (a, Int) -> (a, Int)
    inc x (y, i)
      | x == y    = (y, i + 1)
      | otherwise = (y, i)

{-1.5.1-}
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' = undefined

{-1.5.2-}
iter :: Int -> (a -> a) -> a -> a
iter = undefined

{-1.5.3a-}
pow' :: Int -> Int -> Int
pow' = undefined

{-1.5.3b-}
drop' :: Int -> [a] -> [a]
drop' = undefined

{-1.5.3c-}
replicate' :: Int -> a -> [a]
replicate' = undefined

{-1.5.4-}
foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' = undefined

{-1.5.5a-}
map' :: (a -> b) -> [a] -> [b]
map' = undefined

{-1.5.5b-}
filter' :: (a -> Bool) -> [a] -> [a]
filter' = undefined

{-1.5.6a-}
compose :: [(a -> a)] -> a -> a
compose = undefined

{-1.5.6b-}
fib :: Integer -> Integer
fib = undefined

{-1.6.1
  - f1, f2:
  - f1, f3:
  - f1, f4:
  - f2, f3:
  - f2, f4:
  - f3, f4:
-}

{-1.6.2
  (a) (++ [1]) ==
  (b) (++) [1] ==
-}

{-1.7.1a-}
halfEven :: [Int] -> [Int] -> [Int]
halfEven = undefined

{-1.7.1b-}
halfEven' :: [Int] -> [Int] -> [Int]
halfEven' = undefined

{-1.7.1c-}
halfEven'' :: [Int] -> [Int] -> [Int]
halfEven'' = undefined


{-2.1.1-}
mult :: Integer -> Integer -> Integer
mult = undefined

{-2.1.2-}
sqrt :: Integer -> Integer
sqrt = undefined

{-2.1.3-}
argMax :: (Integer -> Integer) -> Integer -> Integer
argMax = undefined

{-2.1.4-}
mergeSort :: [Integer] -> [Integer]
mergeSort = undefined

{-2.1.5-}
concat'' :: [[a]] -> [a]
concat'' = undefined

{-2.1.6-}
sum' :: [Integer] -> Integer
sum' = undefined

{-2.2.1-}
toSet :: [Integer] -> [Integer]
toSet = undefined

{-2.2.2-}
quicksort :: Ord a => [a] -> [a]
quicksort = undefined

{-2.2.3-}
matches :: Integer -> [Integer] -> [Integer]
matches = undefined

elem' :: Integer -> [Integer] -> Bool
elem' = undefined

{-2.2.4-}
duplicate :: String -> Integer -> String
duplicate = undefined

{-2.3.1-}
prop_elem' :: Int -> [Int] -> Bool
prop_elem' = undefined

{-2.3.2-}
isSet :: [Integer] -> Bool
isSet = undefined

prop_toSet :: [Integer] -> Bool
prop_toSet = undefined

{-2.3.3-}
g :: Integer -> Integer
g n = if n < 10 then n*n else n

argMaxG :: Integer -> Integer
argMaxG = undefined

prop_argMax :: Integer -> Bool
prop_argMax = undefined

{-2.3.4-}
sortP :: (Ord a, Eq b) => [(a,b)] -> [(a,b)]
sortP xs = aux xs []
  where
    aux :: (Ord a, Eq b) => [(a,b)] -> [(a,b)] -> [(a,b)]
    aux [] ys = ys
    aux (x:xs) ys = aux xs $ insert x ys
    insert :: Ord a => (a,b) -> [(a,b)] -> [(a,b)]
    insert x [] = [x]
    insert (x,a) ((y,b):ys)
      | x < y = (x,a) : (y,b) : ys
      | otherwise = (y,b) : insert (x,a) ys

{-2.3.5-}
stutt :: [Int] -> [Int]
stutt xs = concat [concat $ replicate x [x] | x <- xs]

{-2.3.6-}
takeAny :: [a] -> (a,[a])
takeAny (x:xs) = (x,xs)

{-2.3.7-}
h :: [Float] -> [Float]
h [] = []
h (x:xs) = (1 / x) : h xs

{-2.4.1-}
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' = undefined

{-2.4.2-}
iterWhile :: (a -> a -> Bool) -> (a -> a) -> a -> a
iterWhile = undefined

{-2.4.3-}
fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint = undefined

{-2.4.4-}
findSup :: Ord a => (a -> a) -> a -> a -> a
findSup = undefined

{-2.4.5a-}
length' :: [a] -> Integer
length' = undefined

{-2.4.5b-}
reverse' :: [a] -> [a]
reverse' = undefined

{-2.4.5c-}
map'' :: (a -> b) -> [a] -> [b]
map'' = undefined

{-2.4.5d-}
inits' :: [a] -> [[a]]
inits' = undefined

{-2.4.6-}
slope :: (Float -> Float) -> (Float -> Float)
slope = undefined

{-2.4.7-}
integrate :: (Float -> Float) -> (Float -> Float -> Float)
integrate = undefined

{-2.5.1
  \xs ys -> reverse xs ++ ys ==
-}

{-2.6.1-}
lups ::  Ord a => [a] -> [a]
lups = undefined

{-2.6.2a-}
zip'' :: [a] -> [b] -> [(a,b)]
zip'' = undefined
unzip'' :: [(a,b)] -> ([a],[b])
unzip'' = undefined

{-2.6.2b-}
-- see 1.2.3 for zip'
unzip' :: [(a,b)] -> ([a],[b])
unzip' = undefined

{-2.6.2c-}
zip''' :: [a] -> [b] -> [(a,b)]
zip''' = undefined
unzip''' :: [(a,b)] -> ([a],[b])
unzip''' = undefined

{-2.6.3a-}
f :: [Int] -> [Int]
f = undefined

{-2.6.3b-}
f' :: [Int] -> [Int]
f' = undefined

{-2.6.3c-}
f'' :: [Int] -> [Int]
f'' = undefined
