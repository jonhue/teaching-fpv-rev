module ProblemSet_1 where

import Data.List (delete,nub,sort,sortBy)
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
  where ys = map snd (occs xs)

{-1.5.1-}
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p [] = []
takeWhile' p (x:xs)
  | p x       = x : takeWhile' p xs
  | otherwise = []

{-1.5.2-}
iter :: Int -> (a -> a) -> a -> a
iter n f x
  | n <= 0    = x
  | otherwise = iter (n - 1) f (f x)

{-1.5.3a-}
pow' :: Int -> Int -> Int
pow' n k = iter k (n*) 1

{-1.5.3b-}
drop' :: Int -> [a] -> [a]
drop' n xs = iter n tail' xs
  where
    tail' [] = []
    tail' (x:xs) = xs

{-1.5.3c-}
replicate' :: Int -> a -> [a]
replicate' n x = iter n (x:) []

{-1.5.4-}
foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f z []     = z
foldl' f z (x:xs) = foldl' f (f z x) xs

{-1.5.5a-}
map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

{-1.5.5b-}
filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x ys -> if p x then x : ys else ys) []

{-1.5.6a-}
compose :: [(a -> a)] -> a -> a
compose = foldr (.) id

{-1.5.6b-}
fib :: Integer -> Integer
fib n = fst $ foldr (\_ (pprev,prev) -> (prev,pprev+prev)) (0,1) [1..n]

{-1.6.1
  - f1, f2: not equivalent, f1 and f2 have different types
  - f1, f3: equivalent, pattern in f1 replaced by lambda expressions in f3
  - f1, f4: not equivalent, f1 and f4 have different types
  - f2, f3: not equivalent, f2 and f3 have different types
  - f2, f4: not equivalent, f2 and f4 have different types
  - f3, f4: not equivalent, f3 and f4 have different types
-}

{-1.6.2
  (a) (++ [1]) == \xs -> xs ++ [1]
  (b) (++) [1] == \xs -> [1] ++ xs
-}

{-1.7.1a-}
halfEven :: [Int] -> [Int] -> [Int]
halfEven xs ys = [(x + y) `div` 2 | (x,y) <- zip xs ys, even $ x + y]

{-1.7.1b-}
halfEven' :: [Int] -> [Int] -> [Int]
halfEven' [] _ = []
halfEven' _ [] = []
halfEven' (x:xs) (y:ys)
  | even $ x + y =
    let halvedSum = (x + y) `div` 2
    in halvedSum : halfEven' xs ys
  | otherwise    = halfEven' xs ys

{-1.7.1c-}
halfEven'' :: [Int] -> [Int] -> [Int]
halfEven'' xs ys =
  let sums     = map (\(x,y) -> x + y) $ zip xs ys
      evenSums = filter even sums
  in map (`div` 2) evenSums


{-2.1.1-}
mult :: Integer -> Integer -> Integer
mult 0 n = 0
mult m n | n > 0 = n + mult (m - 1) n

{-2.1.2-}
sqrt' :: Integer -> Integer
sqrt' n | n > 0 = aux n
  where
    aux 1 = 1
    aux m
      | m ^ 2 <= n = m
      | otherwise  = aux $ m - 1

{-2.1.3-}
argMax :: (Integer -> Integer) -> Integer -> Integer
argMax _ 0 = 0
argMax g n | n > 0 =
  let rec_max = argMax g (n - 1)
  in if g rec_max > g n then rec_max else n

{-2.1.4-}
toSet :: [Integer] -> [Integer]
toSet xs
  | null xs = []
  | otherwise =
    if head xs `elem` tail xs
    then toSet $ tail xs
    else head xs : (toSet $ tail xs)

{-2.1.5-}
splitList :: [a] -> ([a],[a])
splitList xs = (take n xs, drop n xs)
  where n = length xs `div` 2

mergeLists :: ([Integer],[Integer]) -> [Integer]
mergeLists ([],bs) = bs
mergeLists (as,[]) = as
mergeLists (a:as,b:bs)
  | a < b     = a : mergeLists (as,b:bs)
  | otherwise = b : mergeLists (a:as,bs)

mergeSort :: [Integer] -> [Integer]
mergeSort xs
  | length xs <= 1 = xs
  | otherwise      =
    let (as,bs) = splitList xs
        as'     = mergeSort as
        bs'     = mergeSort bs
    in mergeLists (as',bs')

{-2.1.6-}
concat'' :: [[a]] -> [a]
concat'' = go []
  where
    go acc [] = reverse acc
    go acc (xs:xss) = go (reverse xs ++ acc) xss

{-2.1.7-}
sum' :: [Integer] -> Integer
sum' = go 0
  where
    go acc [] = acc
    go acc (x:xs) = go (acc + x) xs

{-2.2.1-}
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let as = [y | y <- xs, y <= x]
      bs = [y | y <- xs, y > x]
  in quicksort as ++ [x] ++ quicksort bs

{-2.2.2-}
duplicate :: String -> Integer -> String
duplicate xs n = concat [xs | i <- [1..n]]

{-2.3.1-}
prop_elem' :: Integer -> [Integer] -> Bool
prop_elem' x xs = elem' x xs == elem x xs

{-2.3.2-}
isSet :: [Integer] -> Bool
isSet xs = (length $ toSet xs) == length xs

prop_toSet :: [Integer] -> Bool
prop_toSet xs = isSet $ toSet xs

{-2.3.3-}
g :: Integer -> Integer
g n = if n < 10 then n*n else n

argMaxG :: Integer -> Integer
argMaxG n
  | n >= 10 && n < 81 = 9
  | otherwise = n

prop_argMax :: Integer -> Property
prop_argMax n = n >= 0 ==> argMax g n == argMaxG n

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

-- increasing
prop_sortP1 :: [(Int,Int)] -> Bool
prop_sortP1 xs = isIncr (sortP xs)
  where
    isIncr (x:y:xs) = fst x <= fst y && prop_sortP1 (y:xs)
    isIncr _ = True

-- same set
prop_sortP2 :: [(Int,Int)] -> Bool
prop_sortP2 xs =
  let ys = sortP xs
  in subsetOf xs ys && subsetOf ys xs
  where
    subsetOf [] _ = True
    subsetOf (x:xs) ys = x `elem` ys && subsetOf xs (delete x ys)

{-2.3.5-}
stutt :: [Int] -> [Int]
stutt xs = concat [concat $ replicate x [x] | x <- xs]

{-
  The suite prop_stutt_single + prop_stutt_distr is complete as for any list
  [n_1, . . . , n_n] we can derive a complete specification of `stutt`:

  stutt [n_1, . . . , n_k]
  = stutt [n_1] ++ · · · ++ stutt [n_k]                 (by prop_stutt_distr)
  = replicate n_1 n_1 ++ · · · ++ replicate n_k n_k     (by prop_stutt_single)

  The suites prop_stutt_null + prop_stutt_cons and
  prop_stutt_distr + prop_stutt_cons are also complete.
-}

prop_stutt_single n = stutt [n] == replicate n n
prop_stutt_distr ms ns = stutt ms ++ stutt ns == stutt (ms ++ ns)

{-2.3.6-}
takeAny :: Eq a => [a] -> (a,[a])
takeAny xs =
  let (y:ys) = nub xs
  in (y,ys)

prop_takeAny :: [Int] -> Property
prop_takeAny xs =
  let (y,ys) = takeAny xs
  in xs /= [] ==>
    not (y `elem` ys) &&
    sort (nub xs) == sort (nub (y:ys))

{-2.3.7-}
h :: [Float] -> [Float]
h [] = []
h (x:xs) = (1 / x) : h xs

prop_h :: [Float] -> Property
prop_h xs = all (/= 0) xs ==> map (\x -> 1 / x) xs == h xs

{-2.4.1-}
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p [] = []
dropWhile' p (x:xs)
  | p x       = dropWhile' p xs
  | otherwise = x:xs

{-2.4.2-}
iterWhile :: (a -> a -> Bool) -> (a -> a) -> a -> a
iterWhile test f x =
  let x' = f x in
  if test x x' then iterWhile test f x'
               else x

{-2.4.3-}
fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint = iterWhile (/=)

{-2.4.4-}
findSup :: Ord a => (a -> a) -> a -> a -> a
findSup f m = iterWhile (const (<=m)) f

{-2.4.5a-}
length' :: [a] -> Integer
length' = foldr (const (+1)) 0

{-2.4.5b-}
reverse' :: [a] -> [a]
reverse' = foldr (\x -> (++[x])) []

{-2.4.5c-}
inits' :: [a] -> [[a]]
inits' xs = snd $ foldr (\_ (xs,acc) -> (init xs,xs:acc)) (xs,[]) [0..length' xs]

{-2.4.6-}
slope :: (Float -> Float) -> (Float -> Float)
slope f x =
  let h = 0.1
  in (f (x + h) - f x) / h

{-2.4.7-}
integrate :: (Float -> Float) -> (Float -> Float -> Float)
integrate f a b =
  let h = 0.1
      d = b - a
      n = d / h
  in sum [rect h i | i <- [0..n - 1]]
  where
    rect h i = (f (a + h*i + h/2)) * h

{-2.5.1
  \xs ys -> reverse xs ++ ys == (++) . reverse
-}

{-2.6.1-}
ups :: Ord a => [a] -> [a] -> [[a]]
ups [] ys = [reverse ys]
ups (x:xs) [] = ups xs [x]
ups (x:xs) (y:ys)
  | x > y = ups xs (x:y:ys)
  | otherwise = reverse (y:ys) : ups xs [x]

longest :: [[a]] -> [a]
longest [] = []
longest (xs:xss) = if length xs > length ys then xs else ys
  where ys = longest xss

lups :: Ord a => [a] -> [a]
lups xs = longest (ups xs [])

{-2.6.2a-}
-- see 1.2.3 for zip'
unzip' :: [(a,b)] -> ([a],[b])
unzip' [] = ([],[])
unzip' ((x,y):zs) =
  let (xs,ys) = unzip' zs
  in (x:xs,y:ys)

{-2.6.2b-}
zip'' :: [a] -> [b] -> [(a,b)]
zip'' xs ys
  | length xs == length ys = [(xs !! i, ys !! i) | i <- [0..length xs - 1]]
  | otherwise              = []
unzip'' :: [(a,b)] -> ([a],[b])
unzip'' xs = ([x | (x,_) <- xs], [y | (_,y) <- xs])

{-2.6.2c-}
zip''' :: [a] -> [b] -> [(a,b)]
zip''' xs ys
  | length xs == length ys = map (\i -> (xs !! i, ys !! i)) [0..length xs - 1]
  | otherwise              = []
unzip''' :: [(a,b)] -> ([a],[b])
unzip''' = foldr (\(x,y) (xs,ys) -> (x:xs,y:ys)) ([],[])

{-2.6.3a-}
f :: [Int] -> [Int]
f [] = []
f (x:xs)
  | x < 0 = abs x : f xs
  | otherwise = f xs

{-2.6.3b-}
f' :: [Int] -> [Int]
f' xs = [abs x | x <- xs, x < 0]

{-2.6.3c-}
f'' :: [Int] -> [Int]
f'' = map abs . filter ( <0)
