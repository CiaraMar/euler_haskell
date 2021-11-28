module Euler67(euler67) where

import           Control.Arrow   (Arrow ((***)))
import           Control.Monad   (liftM2)
import           Data.List.Split (splitOn)
import           Lib             (windows)

{-
>>> take 3 <$> euler67Data
[[59],[73,41],[52,40,9]]
-}
euler67Data :: IO [[Int]]
euler67Data = parseData <$> readFile "E:\\Programming Projects\\Haskell\\euler\\src\\p067_triangle.txt"

parseData :: String -> [[Int]]
parseData = map (map read . splitOn " ") . splitOn "\n"

sumStep :: Integral a => [(a, [a])] -> [a] -> [(a, [a])]
sumStep = zipWith combine . windowed
    where
        windowed = let e = (0, []) in windows 2 . (++[e]) . (e:)
        combine = flip (liftM2 (***) (+) (:)) . maximum

{-
>>> euler67
(7273,[95,98,99,57,82,53,89,98,24,72,83,91,66,65,65,87,51,85,94,47,65,96,64,87,82,60,77,73,87,37,68,95,88,73,99,89,87,81,55,74,84,55,18,7,70,90,78,91,38,98,52,81,96,80,47,89,99,73,69,53,94,93,71,31,91,84,90,42,54,89,8,94,71,68,77,71,54,95,93,90,65,62,36,97,55,97,82,86,32,81,79,81,81,92,57,87,53,52,73,59])
-}
euler67 :: IO (Int, [Int])
euler67 = maximum . foldl sumStep [] <$> euler67Data
