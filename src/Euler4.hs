{-
Start with 998899 because 999999 is too big
>>> take 5 palindromes
[999999,998899,997799,996699,995599]
-}
palindromes :: [Integer]
palindromes = [(sum . zipWith (*) (iterate (* 10) 1)) [x, y, z, z, y, x] | x <- [9, 8 .. 0], y <- [9, 8 .. 0], z <- [8, 7 .. 0]]

{-Assume that palindrome is at least bigger than 949^2-}
hasTripleDivisor :: Integral a => a -> Bool
hasTripleDivisor n = any (\x -> mod n x == 0) [999, 998..949]

{-
>>> euler4
906609
-}
euler4 :: Integer
euler4 = head . filter hasTripleDivisor $ palindromes
