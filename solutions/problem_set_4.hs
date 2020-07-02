module ProblemSet_4 where

import System.Random (randomRIO)
import Text.Read (readMaybe)


{-1.1.1-}
getInt :: IO (Maybe Int)
getInt = do
  x <- getLine
  return $ readMaybe x

{-1.1.2-}
guessNum :: IO Int
guessNum = do
  rnd <- randomRIO (0,100)
  putStrLn "Guess a number between 0 and 100"
  doGuessNum rnd 1
    where
      doGuessNum :: Int -> Int -> IO Int
      doGuessNum rnd cnt = do
        num <- getInt
        handleNum num rnd cnt
      handleNum :: Maybe Int -> Int -> Int -> IO Int
      handleNum Nothing rnd cnt = do
        putStrLn "Invalid input"
        doGuessNum rnd cnt
      handleNum (Just num) rnd cnt =
        if num < rnd then do
          putStrLn "The number you are looking for is greater"
          doGuessNum rnd (cnt + 1)
        else if num > rnd then do
          putStrLn "The number you are looking for is smaller"
          doGuessNum rnd (cnt + 1)
        else do
          putStrLn "You found it!"
          return cnt

{-1.1.3-}
match :: IO ()
match = play 10 $ cycle [1,2]

readNum :: IO Int
readNum = do
  x <- readLn
  if 1 <= x && x <= 5 then
    return x
  else do
    putStrLn "The input must be between 1 and 5."
    readNum

play :: Show a => Int -> [a] -> IO ()
play n (p:ps) = do
  putStrLn $ "Matches: " ++ show n ++ ". Player " ++ show p ++ "?"
  m <- readNum
  if n <= m then
    putStrLn $ "Player " ++ show p ++ " wins!"
  else
    play (n - m) ps

{-1.2.1
  (a) 1 + (2 * 3)
      - 2 * 3                        [innermost + outermost]
  (b) (1 + 2) * (2 + 3)
      -   1 + 2                      [innermost + outermost]
      -   2 + 3                      [innermost + outermost]
  (c) fst (1 + 2, 2 + 3)
      -   1 + 2                      [innermost]
      -   2 + 3                      [innermost]
      -   fst (1 + 2, 2 + 3)         [outermost]
  (d) fst (snd (1, 2 + 3), 4)
      -   2 + 3                      [innermost]
      -   snd (1, 2 + 3)             [neither]
      -   fst (snd (1, 2 + 3), 4)    [outermost]
  (e) (\x -> 1 + x) (2 * 3)
      -   2 * 3                      [innermost]
      -   (\x -> 1 + x) (2 * 3)      [outermost]
-}

{-1.2.2
  map (*2) (1 : threes) !! 1
  = ((*2) 1 : map (*2) threes) !! 1
  = map (*2) threes !! (1-1)
  = map (*2) (3 : threes) !! (1-1)
  = ((*2) 3 : map (*2) threes) !! (1-1)
  = ((*2) 3 : map (*2) threes) !! 0
  = (*2) 3
  = 3*2
  = 6

  (\f -> \x -> x + f 2) (\y -> y * 2) (3 + 1)
  = (\x -> x + (\y -> y * 2) 2) (3 + 1)
  = (3 + 1) + (\y -> y * 2) 2
  = 4 + (\y -> y * 2) 2
  = 4 + 2 * 2
  = 4 + 4
  = 8

  head (filter (/=3) threes)
  = head (filter (/=3) (3 : threes))
  = head (filter (/=3) threes)
  = ...
-}

{-1.3.1-}
nats :: [Integer]
nats = aux 0
  where
    aux :: Integer -> [Integer]
    aux n = n : aux (n + 1)

enumFromThen' :: Integer -> Integer -> [Integer]
enumFromThen' a b =
  let d = b - a in
  [a + d*i | i <- nats]

enumFromThenTo' :: Integer -> Integer -> Integer -> [Integer]
enumFromThenTo' a b c = takeWhile (<c) $ enumFromThen' a b

{-1.3.2-}
fib :: [Integer]
fib = 0 : 1 : zipWith (+) fib (tail fib)

{-1.3.3-}
fib' :: [Integer]
fib' = map f [0..]
  where
    f 0 = 0
    f 1 = 1
    f n = fib' !! (n - 1) + fib' !! (n - 2)

{-1.3.3a-}
zipWithN :: ([a] -> b) -> [[a]] -> [b]
zipWithN f xs
  | null xs || any null xs = []
  | otherwise              = f (map head xs) : zipWithN f (map tail xs)

nonacci :: Int -> [Integer]
nonacci n = replicate (n - 1) 0 ++ [1] ++ nonacci'
  where
    iterateN n f = take n . iterate f
    nonacci' = zipWithN sum $ iterateN n tail (nonacci n)

{-1.3.3b-}
nonacci' :: Int -> [Integer]
nonacci' n = map f [0..]
  where
    f x
      | x < n - 1 = 0
      | x == n - 1 = 1
      | otherwise = sum $ take n $ drop (x - n) (nonacci' n)


{-2.1.1-}
putStr' :: String -> IO ()
putStr' "" = return ()
putStr' (c:cs) = do
  putChar c
  putStr' cs

putStrLn' :: String -> IO ()
putStrLn' cs = putStr' (cs ++ "\n")

{-2.1.2-}
getLine' :: IO String
getLine' = do
  c <- getChar
  if c == '\n' then
    return ""
  else do
    cs <- getLine'
    return (c:cs)

{-2.1.3-}
fmap :: (a -> b) -> IO a -> IO b
fmap f a = do
  x <- a
  return $ f x

{-2.1.4-}
repeat' :: IO Bool -> IO () -> IO ()
repeat' test oper = do
  b <- test
  if b then
    return ()
  else do
    oper
    repeat' test oper

{-2.1.5-}
whileG :: (a -> IO Bool) -> (a -> IO a) -> (a -> IO a)
whileG cond oper x = do
  b <- cond x
  if b then
    return x
  else do
    x' <- oper x
    whileG cond oper x'

readN :: IO Float
readN = do
  putStrLn "Enter a number:"
  n <- getIntLoop
  (_,acc) <- whileG cond oper (n,0)
  return $ (fromIntegral acc) / (fromIntegral n)
    where
      cond :: (Int,Int) -> IO Bool
      cond (cnt,_) = return $ cnt <= 0
      oper :: (Int,Int) -> IO (Int,Int)
      oper (cnt,acc) = do
        putStr ">>> "
        n <- getIntLoop
        return (cnt - 1, acc + n)
      getIntLoop :: IO Int
      getIntLoop = do
        n <- getInt
        case n of
          Nothing -> do
            putStrLn "Invalid input"
            getIntLoop
          Just n -> return n

{-2.1.6-}
accumulate :: [IO a] -> IO [a]
accumulate [] = return []
accumulate (a:as) = do
  x <- a
  xs <- accumulate as
  return (x:xs)

sequence :: [IO a] -> IO ()
sequence as = do
  xs <- accumulate as
  return ()

seqList :: [a -> IO a] -> a -> IO a
seqList [] x = return x
seqList (a:as) x = do
  x' <- a x
  seqList as x'

{-2.1.7-}
vowelCounter :: IO ()
vowelCounter = count 0
  where
    count :: Int -> IO ()
    count n = do
      s <- getLine
      let n' = n + length (filter (`elem` "aeiou") s)
      putStrLn ("vowels: " ++ show n')
      count n'

{-2.1.8-}
toDecimal :: String -> Integer -> Integer
toDecimal [] _ = 0
toDecimal ('0':bs) r = toDecimal bs (r * 2)
toDecimal ('1':bs) r = r + toDecimal bs (r * 2)

main :: IO ()
main = do
  bin <- getLine
  let (pref,num) = splitAt 2 bin
  if pref /= "0b" || any (\b -> b `notElem` ['0', '1']) num then
    putStrLn "Invalid input"
  else
    print $ toDecimal (reverse num) 1
  main

{-2.2.1
  a = [True]
  b = []

  In general: if `a` is not empty, and `b` is empty.
-}

{-2.2.2
  (a) head (map (\x -> x * x) [1,2,3])
      = head ((\x -> x * x) 1 : map (\x -> (x * x) [2, 3]))
      = (\x -> x * x) 1
      = 1 * 1
      = 1

  (b) let ones = 1 : ones, then:
      elem 0 (ones ++ [0])
      = elem 0 ((1 : ones ) ++ [0])
      = elem 0 (1 : (ones ++ [0]))
      = elem 0 (ones ++ [0])
      = ...
-}

{-2.2.3
  (a) (\f g -> g . map f) (+1) head odds
      = (\g -> g . map (+1)) head odds
      = (head . map (+1)) odds
      = (\x -> head (map (+1) x)) odds
      = head (map (+1) odds)
      = head (map (+1) (1 : map (+2) odds))
      = head (((+1) 1) : map (+1) (map (+2) odds))
      = (+1) 1
      = 2

  (b) False || inf == inf
      = inf == inf
      = inf == inf
      = ...
-}

{-2.3.1a-}
sieve :: [Integer] -> [Integer]
sieve (x:xs) = x : sieve [y | y <- xs, y `mod` x > 0]

{-2.3.1b-}
primes :: [Integer]
primes = sieve [2..]

{-2.3.2-}
factors :: Integer -> [Integer]
factors n = [m | m <- [1..n], n `mod` m == 0]

prime :: Integer -> Bool
prime n = factors n == [1,n]

hamming :: [Integer]
hamming = [n | n <- [1..], all aux $ factors n]
  where
    aux n = not (prime n) || n `elem` [2,3,5]
