module Lib where

import Data.Numbers.Primes (wheelSieve)

{-
>>> take 20 primes
[2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71]
-}
primes :: [Integer]
primes = wheelSieve 100

{-
>>> multiplePrimeFactors 60
[2,2,3,5]

>>> multiplePrimeFactors 232792560
[2,2,2,2,3,3,5,7,11,13,17,19]
-}
multiplePrimeFactors :: Integer -> [Integer]
multiplePrimeFactors n = factor n primes
                where
                    factor _ [] = []
                    factor m (p:xs)
                        | p > m = []
                        | mod m p == 0 = p:factor (div m p) (p:xs)
                        | otherwise = factor m xs

merge :: [a] -> [a] -> [a]
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) = x : y : merge xs ys

{-
>>> lcmSequence !! 20
232792560
-}
lcmSequence :: [Integer]
lcmSequence = scanl1 lcm [1..]

sumToN :: Integral a => a -> a
sumToN n = (n * (n + 1)) `div` 2

squareSumToN :: Integral a => a -> a
squareSumToN n = (n * (n + 1) * (2 *n + 1)) `div` 6

square :: Num a => a -> a
square n = n * n
