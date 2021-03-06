module Euler17(euler17) where
import           Data.Char (digitToInt)

one = ["one","two","three","four","five","six","seven","eight",
     "nine","ten","eleven","twelve","thirteen","fourteen","fifteen",
     "sixteen","seventeen","eighteen", "nineteen"]
ty = ["twenty","thirty","forty","fifty","sixty","seventy","eighty","ninety"]

decompose x
    | x == 0                       = []
    | x < 20                       = one !! (x-1)
    | x >= 20 && x < 100           =
        ty !! (firstDigit x - 2) ++ decompose ( x - firstDigit x * 10)
    | x < 1000 && x `mod` 100 ==0  =
        one !! (firstDigit x-1) ++ "hundred"
    | x > 100 && x <= 999          =
        one !! (firstDigit x-1) ++ "hundredand" ++decompose ( x - firstDigit x * 100)
    | x == 1000                    = "onethousand"
    | otherwise = []

  where firstDigit x = digitToInt . head . show $ x

{-
>>> euler17
21124
-}
euler17 = length . concatMap decompose $ [1..1000]
