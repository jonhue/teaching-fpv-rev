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
enumFromThenTo' a b c = aux $ enumFromThen' a b
  where
    aux :: [Integer] -> [Integer]
    aux (x:xs)
      | x <= c    = x : aux xs
      | otherwise = []
