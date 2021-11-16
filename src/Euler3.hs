module Euler3 where

import Data.Numbers.Primes (primeFactors)

{-
>>> euler3
6857
-}
euler3 :: Integer
euler3 = last $ primeFactors 600851475143 
