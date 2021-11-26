module Euler16 where

import Lib(listFromNum)

{-
>>> euler16
1366
-}
euler16 :: Integer
euler16 = sum . listFromNum $ 2^1000
