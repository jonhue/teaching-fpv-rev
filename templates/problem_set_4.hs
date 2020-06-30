module ProblemSet_4 where

import System.Random (randomRIO)
import Text.Read (readMaybe)


{-1.1.1-}
getInt :: IO (Maybe Int)
getInt = undefined

{-1.1.2-}
guessNum :: IO Int
guessNum = undefined

{-1.1.3-}
match :: IO ()
match = undefined

{-1.2.1
  (a) 1 + (2 * 3)
  (b) (1 + 2) * (2 + 3)
  (c) fst (1 + 2, 2 + 3)
  (d) fst (snd (1, 2 + 3), 4)
  (e) (\x -> 1 + x) (2 * 3)
-}

{-1.2.2
  map (*2) (1 : threes) !! 1
  = ...

  (\f -> \x -> x + f 2) (\y -> y * 2) (3 + 1)
  = ...

  head (filter (/=3) threes)
  = ...
-}

{-1.3.1-}
enumFromThen' :: Integer -> Integer -> [Integer]
enumFromThen' = undefined

enumFromThenTo' :: Integer -> Integer -> Integer -> [Integer]
enumFromThenTo' = undefined

{-1.3.2-}
fib :: [Integer]
fib = undefined

{-1.3.3-}
fib' :: [Integer]
fib' = map f [0..]
  where
    f 0 = 0
    f 1 = 1
    f n = fib' !! (n - 1) + fib' !! (n - 2)

{-1.3.3a-}
zipWithN :: ([a] -> b) -> [[a]] -> [b]
zipWithN = undefined

nonacci :: Int -> [Integer]
nonacci = undefined

{-1.3.3b-}
nonacci' :: Int -> [Integer]
nonacci' = undefined


{-2.1.1-}
putStr' :: String -> IO ()
putStr' = undefined

putStrLn' :: String -> IO ()
putStrLn' = undefined

{-2.1.2-}
getLine' :: IO String
getLine' = undefined

{-2.1.3-}
fmap :: (a -> b) -> IO a -> IO b
fmap = undefined

{-2.1.4-}
repeat' :: IO Bool -> IO () -> IO ()
repeat' = undefined

{-2.1.5-}
whileG :: (a -> IO Bool) -> (a -> IO a) -> (a -> IO a)
whileG = undefined

readN :: IO Float
readN = undefined

{-2.1.6-}
accumulate :: [IO a] -> IO [a]
accumulate = undefined

sequence :: [IO a] -> IO ()
sequence = undefined

seqList :: [a -> IO a] -> a -> IO a
seqList = undefined

{-2.1.7-}
vowelCounter :: IO ()
vowelCounter = undefined

{-2.1.8-}
main :: IO ()
main = undefined

{-2.2.1
  a =
  b =
-}

{-2.2.2
  (a) head (map (\x -> x * x) [1,2,3])
      = ...

  (b)
-}

{-2.2.3
  (a) (\f g -> g . map f) (+1) head odds
      = ...

  (b) False || inf == inf
      = ...
-}

{-2.3.1a-}
sieve :: [Integer] -> [Integer]
sieve = undefined

{-2.3.1b-}
primes :: [Integer]
primes = undefined

{-2.3.2-}
factors :: Integer -> [Integer]
factors = undefined

hamming :: [Integer]
hamming = undefined
