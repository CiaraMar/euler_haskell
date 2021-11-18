module Euler9 where

import           Lib (factors)

{-
a = m^2 - n^2, b = 2mn, c = m^2 + n^2, a + b + c = 1000
=> 2m^2 + 2mn = 1000
=> m(n+m) = 500
=> m | 500 and n + m | 500
=> 500 = mx, 500 = y(n + m)
=> mx = my + yn
=> m(x-y) = yn
=> m | y or m | n
=> mx = nz = 500
-}

{-
>>> euler9
31875000
-}
euler9 :: Integer
euler9 = head [a*b*c | m <- possibleValues, n <- possibleValues, let a = m^2 - n^2, let b = 2*m*n, let c = m^2 + n^2, a + b + c == 1000]
    where
        possibleValues = factors 500
