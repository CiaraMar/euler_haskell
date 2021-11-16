import           Lib (squareSumToN, sumToN, square)

{-
>>> euler6
25164150
-}
euler6 :: Integer
euler6 = (square . sumToN) 100 - squareSumToN 100
