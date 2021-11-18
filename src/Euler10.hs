module Euler10 where

import Lib(primes)

{-
>>> euler10
142913828922
-}
euler10 :: Integer
euler10 = sum (takeWhile (< 2000000) primes)
