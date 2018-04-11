module EulerMath
  ( prime_factors
  , prime
  , pheo
  , owl
  , divisible
  , divisibleBy
  , eratosthenes) where

prime_factors_recurse :: (Integral a) => a -> a -> [a] -> [a]
prime_factors_recurse 1 _ results = results
prime_factors_recurse n i results
  | n `mod` i == 0 = prime_factors_recurse (n `div` i) i (i:results)
  | otherwise = prime_factors_recurse n (i + 1) results

prime_factors :: (Integral a) => a -> [a]
prime_factors n = prime_factors_recurse n 2 []

-- O(sqrt(n)) prime-checking algorithm
prime :: (Integral a) => a -> Bool
prime 1 = False
prime n = let possible_factors = takeWhile (<= (floor. sqrt . fromIntegral) n) [2..n]
          in all id $ map ((/= 0) . (n `mod`)) possible_factors

-- The Phoenix combinator, turns a binary function and two unary functions into
-- a binary function (by applying the unaries to the argument on each side of
-- the binary)
pheo f g h x = f (g x) (h x)
owl = ((.).(.))

divisible :: (Integral a) => a -> a -> Bool
divisible = owl (== 0) mod

divisibleBy :: (Integral a) => a -> a -> Bool
divisibleBy = flip divisible

-- divisibleBy x y = (y `mod` x) == 0

eratosthenesHelper :: (Integral a) => [a] -> [Bool] -> [a] -> [a]
eratosthenesHelper [] [] is = is
eratosthenesHelper nums marks primes@(p:ps)
  | null filtered = primes
  | otherwise = 
    where filtered = [nums !! (n - 2) | n <- nums, marks !! (n - 2), n > p]


-- primes [] = primes
-- eratosthenesHelper primes@(p:_) rest
--   | null filtered = primes
--   | otherwise = eratosthenesHelper ((head filtered) : primes) (tail filtered)
--   where filtered = filter (not . (divisibleBy p)) rest

eratosthenes :: (Integral a) => a -> [a]
eratosthenes n
  | n <= 1 = []
  | otherwise = eratosthenesHelper [2..n] ([True] ++ [i `mod` 2 /= 2 | i <- [3..n]]) [2]

siv (p:xs) = p : siv [x | x <- xs, x `mod` p /= 0]

eratosthenes n = siv [2..n]
