{-
>>> take 10 primes
[2,3,5,7,11,13,17,19,23,29]
-}
primes :: [Integer]
primes = sieve [2..]
        where
            sieve (p:xs) = p : sieve [x | x <- xs, rem x p > 0]
            sieve [] = []

{-
>>> primeFactors 60
[2,3,5]

>>> primeFactors 600851475143
[71,839,1471,6857]
-}
primeFactors :: Integer -> [Integer]
primeFactors n = factor n primes
                where 
                    factor _ [] = []
                    factor m (p:xs)
                        | p > m = []
                        | mod m p == 0 = p:factor (div m p) xs
                        | otherwise = factor m xs


{-
>>> euler3
6857
-}
euler3 :: Integer
euler3 = last $ primeFactors 600851475143 
