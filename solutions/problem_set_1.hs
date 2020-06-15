module ProblemSet_1 where

import Data.List (nub,sort,sortBy)
import Test.QuickCheck


{-1.1.1
  Behavior of `mystery`: returns False iff all arguments are equal, else True.
-}

{-1.1.2-}
threeDifferent :: Integer -> Integer -> Integer -> Bool
threeDifferent m n p = m /= n && m/= p && n /= p

{-1.2.1-}
pow :: Integer -> Integer -> Integer
pow x 0 = 1
pow x n | n > 0 = x * pow x (n - 1)

{-1.2.2-}
ascending :: Ord a => [a] -> Bool
ascending [] = True
ascending [_] = True
ascending (x:y:xs) = x <= y && ascending (y:xs)

{-1.2.3-}
zip' :: [a] -> [b] -> [(a,b)]
zip' (x:xs) (y:ys) = (x,y) : zip xs ys
zip' _ _ = []

{-1.2.4-}
insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)
  where
    insert :: Ord a => a -> [a] -> [a]
    insert x [] = [x]
    insert x (y:ys)
      | x <= y    = x : y : ys
      | otherwise = y : insert x ys

{-1.2.5
  (a) tail recursive: the last call is 'prod', all other calculations happen in the
      arguments
  (b) not tail recursive: after the call to 'prod', there is a multiplication
  (c) tail recursive: the last call is 'prod', just as in (a). The 'if-then-else'
      is not relevant here because the condition is calculated before the
      recursive call. After the call no further work needs to be done.
-}

{-1.2.6-}
fac :: Int -> Int
fac n = go 1 n
  where
    go :: Int -> Int -> Int
    go acc n
      | n > 0     = go (n * acc) (n - 1)
      | otherwise = acc

{-1.2.7-}
regions :: Integer -> Integer
regions 0 = 1
regions n | n > 0 = n + regions (n - 1)

{-1.2.8-}
remdups :: Eq a => [a] -> [a]
remdups = f []
  where
    f acc [] = acc
    f acc (x:xs)
      | x `elem` acc = f acc xs
      | otherwise    = f (acc ++ [x]) xs

{-1.3.1-}
concat' :: [[a]] -> [a]
concat' xss = [x | xs <- xss, x <- xs]

{-1.3.2-}
factors :: Int -> [Int]
factors n = [m | m <- [1..n], n `mod` m == 0]

prime :: Int -> Bool
prime n = factors n == [1, n]

primes :: Int -> [Int]
primes n = [p | p <- [1..n], prime p]

{-1.3.3-}
matches :: Integer -> [Integer] -> [Integer]
matches n xs = [x | x <- xs, x == n]

elem' :: Integer -> [Integer] -> Bool
elem' x xs = length (matches x xs) > 0

{-1.4.1-}
prop_zip' :: [Int] -> [Int] -> Bool
prop_zip' xs ys = zip' xs ys == zip xs ys

prop_concat' :: [[Int]] -> Bool
prop_concat' xss = concat' xss == concat xss

{-1.4.2-}
occs :: Eq a => [a] -> [(a, Int)]
occs xs = sortBy (\(_,a) (_,b) -> compare b a) $ map aux $ nub xs
  where
    aux x = (x, count x xs)
    count x xs = length (filter (== x) xs)

-- number of recurrences are correct
prop_occs1 :: [Int] -> Bool
prop_occs1 xs = all (\(x,n) -> n > 0 && count x xs == n) ys
  where
    ys = occs xs
    count x xs = length (filter (== x) xs)

-- every element appears only once
prop_occs2 :: [Int] -> Bool
prop_occs2 xs = let ys = map fst (occs xs) in nub ys == ys

-- sorted in decreasing order by number of recurrences
prop_occs3 :: [Int] -> Bool
prop_occs3 xs = sort ys == reverse ys
  where
    ys = map snd (occs xs)

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
