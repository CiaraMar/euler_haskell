module Euler1
    ( euler1
    ) where


euler1 :: Integer
euler1 = sum [x | x <- [1..999], mod x 3 == 0 || mod x 5 == 0]