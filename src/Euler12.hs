module Euler12 where

import           Lib (factors, primes, numFactors, multiplePrimeFactors)

triangle :: Integral a => a -> a
triangle n = (n * (n + 1)) `div` 2

{-
To have over 500 divisors that means it should have around log2(500) > 8 unique prime factors
Minimum number with 8 unique prime factors is 2*3*5*7*11*13*17*19 == 9,699,690
The sqrt of this is: 4404, which as it's triangle goes to 9,699,810
Which has 64 factors, so we should have a heuristic that increases the n by an amount that would guess closer, then switch to a full search once we're close (i.e. slightly over)

The number of factors is product multiplicity of prime factors +1
So we want to find the closest composite number by varying the number of prime factors up
This can be abstracted just to work with a starting list of n 2's, then slowly replace them in such a way as to get closest to 500 in the product

We convert 500 into it's multiset of prime factors
Then convert those to the minimum number that would be made if those were the multiplicities (+1) of the prime factors of our target number
>>> primeExponentsToNumber . reverse . multiplePrimeFactors $ 500  
Couldn't match type ‘Multiset Integer’ with ‘[Integer]’
Expected type: Integer -> [Integer]
  Actual type: Integer -> Multiset Integer
-}

triangleToN :: (Integral a) => a -> Integer
triangleToN = floor . sqrt . (*2) . fromIntegral

nearestTriangle :: (Integral a) => a -> Integer
nearestTriangle = triangle . triangleToN

firstNPrimes :: Int -> [Integer]
firstNPrimes = flip take primes

{-
>>> minimumNumberByNumberOfFactors 500
9699690
-}
minimumNumberByNumberOfFactors :: (Integral a) => a -> Integer
minimumNumberByNumberOfFactors = product . firstNPrimes . floor . logBase 2 . fromIntegral

{-
>>> take 5 (findEuler12 500)
[9699810,19399620,38799240,77598480,155196960,310393920]
-}
findEuler12 target = heuristic (firstNPrimes 8) start
    where 
        start = nearestTriangle . minimumNumberByNumberOfFactors $ target
        heuristic [] n = [n]
        heuristic (p:candidates) n
            | numFactors n < target = n:heuristic (p:candidates) (nearestTriangle (n * p))
            | otherwise = n:heuristic candidates (nearestTriangle (n `div` p))

primeExponentsToNumber :: [Integer] -> Integer
primeExponentsToNumber = product . zipWith (^) primes . map pred 

{-
>>> euler12
76576500
-}
euler12 :: Integer
euler12 = head . filter ((>500) . numFactors) . map triangle $ [4404..]
