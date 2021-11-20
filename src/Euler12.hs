module Euler12 where

import           Lib (factors, primes)

triangle :: Integral a => a -> a
triangle n = (n * (n + 1)) `div` 2

{-
To have over 500 divisors that means it should have around log2(500) > 8 unique prime factors
Minimum number with 8 unique prime factors is 2*3*5*7*11*13*17*19 == 9,699,690
The sqrt of this is: 4404, which as it's triangle goes to 9,699,810
Which has 64 factors, so we should have a heuristic that increases the n by an amount that would guess closer, then switch to a full search once we're close (i.e. slightly over)
-}

triangeToN :: (Integral a) => a -> Integer
triangeToN = floor . sqrt . (*2) . fromIntegral

nearestTriangle :: (Integral a) => a -> Integer
nearestTriangle = triangle . triangeToN

firstNPrimes :: Int -> [Integer]
firstNPrimes = flip take primes

minimumNumberByNumberOfFactors :: (Integral a) => a -> Integer
minimumNumberByNumberOfFactors = product . firstNPrimes . round . logBase 2 . fromIntegral

numFactors :: Integer -> Int
numFactors = length . factors

{-
>>> euler12
76576500
-}
euler12 :: Integer
euler12 = head . filter ((>500) . numFactors) . map triangle $ [4404..]
