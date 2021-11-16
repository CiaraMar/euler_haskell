module Euler2
    ( euler2
    ) where

{-
>>> fibs !! 5
8
-}
fibs :: [Integer]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

{-
>>> euler2
4613732
-}
euler2 :: Integer
euler2 = sum . takeWhile (< 4000000) . filter even $ fibs
