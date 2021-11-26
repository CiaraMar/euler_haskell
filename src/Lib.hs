module Lib where

import           Control.Arrow
import           Data.Array
import           Data.Function               (fix)
import           Data.List                   (group, nub)
import           Data.Maybe                  (fromMaybe)
import qualified Data.MultiSet               as MS2
import           Data.Numbers.Primes         (wheelSieve)
import qualified Data.PQueue.Min             as PQ
import           Data.Sort                   (sort, sortBy, uniqueSort,
                                              uniqueSortOn)
import qualified Math.Combinatorics.Multiset as MS1

{-
>>> take 20 primes
[2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71]
-}
primes :: [Integer]
primes = wheelSieve 100

{-
>>> multiplePrimeFactors 60
MS {toCounts = [(2,2),(3,1),(5,1)]}

>>> multiplePrimeFactors 62370000
MS {toCounts = [(2,4),(3,4),(5,4),(7,1),(11,1)]}
-}
multiplePrimeFactors :: Integer -> MS1.Multiset Integer
multiplePrimeFactors n = MS1.fromList $ factor n primes
                where
                    factor _ [] = []
                    factor m (p:xs)
                        | p > m = []
                        | mod m p == 0 = p:factor (div m p) (p:xs)
                        | otherwise = factor m xs

merge :: [a] -> [a] -> [a]
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) = x : y : merge xs ys

{-
>>> lcmSequence !! 20
232792560
-}
lcmSequence :: [Integer]
lcmSequence = scanl1 lcm [1..]

sumToN :: Integral a => a -> a
sumToN n = (n * (n + 1)) `div` 2

squareSumToN :: Integral a => a -> a
squareSumToN n = (n * (n + 1) * (2 *n + 1)) `div` 6

square :: Num a => a -> a
square n = n * n

{-
[1,2,3]
[1:ps | ps <- [2:ps | ps <- [3:ps <- [[]]]]]
[1:ps | ps <- [2:ps | ps <- [[3], []]]]
[1:ps | ps <- [[2, 3], [2], [3], []]]
[[1,2,3],[1,2],[1,3],[1],[2,3],[2],[3],[]]
>>> powerset [1,2,3]
[[1,2,3],[1,2],[1,3],[1],[2,3],[2],[3],[]]
-}
powerset :: [a] -> [[a]]
powerset []     = [[]]
powerset (x:xs) = [x:ps | ps <- psx] ++ psx
    where psx = powerset xs

{-
>>> multiPowerSet . fromList $ [2, 2, 5]
[MS {toCounts = []},MS {toCounts = [(5,1)]},MS {toCounts = [(2,1)]},MS {toCounts = [(2,1),(5,1)]},MS {toCounts = [(2,2)]},MS {toCounts = [(2,2),(5,1)]}]
-}
multiPowerSet :: MS1.Multiset a -> [MS1.Multiset a]
multiPowerSet ms = concatMap (`MS1.kSubsets` ms) [0..MS1.size ms]

{-
>>> splits  . fromList $ [5, 5, 2]
[(MS {toCounts = []},MS {toCounts = [(2,1),(5,2)]}),(MS {toCounts = [(5,1)]},MS {toCounts = [(2,1),(5,1)]}),(MS {toCounts = [(5,2)]},MS {toCounts = [(2,1)]}),(MS {toCounts = [(2,1)]},MS {toCounts = [(5,2)]}),(MS {toCounts = [(2,1),(5,1)]},MS {toCounts = [(5,1)]}),(MS {toCounts = [(2,1),(5,2)]},MS {toCounts = []})]

>>> factors 60
[1,2,3,4,5,6,10,12,15,20,30,60]
-}
factors :: Integer -> [Integer]
factors = sort . map (product . MS1.toList) . multiPowerSet . multiplePrimeFactors

fillOffsetZip :: Int -> [a] -> [(Maybe a, a)]
fillOffsetZip length values = zip (replicate length Nothing ++ map Just values) values

{-
>>> fmap fst (buildProduct 13 input)
Just 5000940
-}
buildProduct :: (Eq t, Num t) => Int -> [t] -> Maybe (t, [(Maybe t, t)])
buildProduct length = build 1 . split . fillOffsetZip length
    where
        split = splitAt length
        build _ ([], [])           = Nothing
        build prod ([], rs)        = Just (prod, rs)
        build prod ((_, 0):ls, rs) = build 1 (split (ls ++ rs))
        build prod ((_, l):ls, rs) = build (prod * l) (ls, rs)

{-
>>> slidingProduct 13 input
[5000940,4199040,4898880,9797760,9797760,2177280,13063680,7257600,25401600,50803200,71124480,284497920,568995840,189665280,158054400,112896000,84672000,84672000,381024000,63504000,114307200,65318400,146966400,62985600,62985600,283435200,566870400,1020366720,1632586752,1632586752,2040733440,453496320,78382080,371589120,2972712960,1857945600,2477260800,1651507200,412876800,294912000,221184000,235146240,167961600,188956800,104976000,42865200,342921600,244944000,244944000,69984000,812851200,812851200,270950400,6270566400,14108774400,23514624000,3225600,2419200,604800,967680,2419200,3628800,3628800,677376,225792,28224,4032,12096,72576,145152,145152,290304,373248,1492992,6718464,6718464,3359232,2239488,6718464,20155392,53747712,13063680,3265920,26127360,17418240,27869184,424673280,495452160,495452160,1114767360,836075520,2709504,24385536,27869184,97542144,219469824,768144384,987614208,630118440,120960000,96768000,96768000,2016000,1008000,3024000,10584000,13230000,9922500,59535000,107163000,107163000,428652000,214326000,1500282000,2700507600,3780710640,8821658160,7841473920,4480842240,5377010688,1792336896,2091059712,929359872,313528320,235146240,313528320,940584960,376233984,125411328,55738368,148635648,148635648,1189085184,396361728,509607936,445906944,222953472,74317824,37158912,6193152,9289728,32514048,40642560,30481920,76204800,66679200,365783040,261273600,87091200,62208000,31104000,258048000,258048000,309657600,309657600,495452160,123863040,123863040,371589120,743178240,212336640,297271296,37158912,32514048,5419008,3612672,4214784,4741632,5334336,10668672,42674688,28449792,28449792,28449792,36578304,73156608,83607552,167215104,125411328,627056640,78382080,117573120,39191040,19595520,9797760,1959552,1959552,559872,1119744,279936,1119744,1866240,1658880,9953280,1105920,7741440,30965760,61931520,371589120,247726080,619315200,619315200,928972800,1161216000,2090188800,261273600,87091200,348364800,248832000,186624000,186624000,155520000,77760000,139968000,69984000,163296000,195955200,108864000,435456000,1088640000,1632960000,2612736000,870912000,1161216000,928972800,3715891200,3715891200,928972800,265420800,353894400,566231040,424673280,84934656,56623104,14155776,42467328,223948800,199065600,33177600,33177600,24883200,2048385024,1820786688,910393344,1170505728,462944160,462944160,132269760,208372500,250047000,150028200,100018800,66679200,80015040,102876480,257191200,171460800,24494400,97977600,156764160,44789760,268738560,313528320]
-}
slidingProduct :: (Integral a) => Int -> [a] -> [a]
slidingProduct length = restart
    where
        slide _ ((_, 0):slices) = restart (map snd slices)
        slide prod ((Just a, b):slices) = b':slide b' slices
            where b' = b * div prod a
        slide _ _ = []
        restart ls = case buildProduct length ls of
            Just (newProd, slices) -> newProd:slide newProd slices
            Nothing                -> []

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = left:chunks n right
    where (left, right) = splitAt n xs

applyExps :: (Integral a) => [a] -> [a] -> a
applyExps ps es = product $ zipWith (^) ps es

{-
>>> numFactors 76576500
576
-}
numFactors :: Integer -> Int
numFactors = product . map (+1) . MS1.getCounts . multiplePrimeFactors

filterAccordingTo :: [Bool] -> [a] -> [a]
filterAccordingTo bs xs = map snd . filter fst $ zip bs xs

{-
>>> map length $ take 30 $ [replicate 4 True]:[map (++ [True]) (MS.permutations . MS.fromCounts $ [(True, 4 - 1), (False, i)]) | i <- [1 .. ]]
[1,4,10,20,35,56,84,120,165,220,286,364,455,560,680,816,969,1140,1330,1540,1771,2024,2300,2600,2925,3276,3654,4060,4495,4960]


[
    [[True,True,True,True]],
    [[True,True,True,False,True],[True,True,False,True,True],[True,False,True,True,True],[False,True,True,True,True]],
    [[True,True,True,False,False,True], [True,True,False,True,False,True], [True,False,True,True,False,True], [False,True,True,True,False,True],
    [True,True,False,False,True,True],
    [True,False,False,True,True,True],
    [True,False,True,False,True,True],
    [False,False,True,True,True,True],
    [False,True,True,False,True,True],
    [False,True,False,True,True,True]]
]

>>> take 5 $ kPrimeGroups 1
[[[2]],[[3]],[[5]],[[7]],[[11]]]

TODO make filter creation cheaper by simply adding an extra False where it makes sense (i.e. at the right of all True's)
-}
kPrimeGroups :: Int -> [[[Integer]]]
kPrimeGroups k
    | k == 1 = map (\x -> [[x]]) primes
    | otherwise = [[filterAccordingTo filt primes | filt <- filters] | filters <- filterGroups]
    where
        filterGroups = [replicate k True]:[map (++ [True]) (MS1.permutations . MS1.fromCounts $ [(True, k - 1), (False, i)]) | i <- [1 .. ]]

ms2ToMS1 :: MS2.MultiSet a -> MS1.Multiset a
ms2ToMS1 = MS1.MS . MS2.toOccurList

ms1ToMS2 :: (Ord a) => MS1.Multiset a -> MS2.MultiSet a
ms1ToMS2 = foldr (\(x, n) m -> MS2.insertMany x n m) MS2.empty . MS1.toCounts

{-
>>> kSplits 2 . multiplePrimeFactors $ 576
[(MS {toCounts = [(3,2)]},MS {toCounts = [(2,6)]}),(MS {toCounts = [(2,1),(3,1)]},MS {toCounts = [(2,5),(3,1)]}),(MS {toCounts = [(2,2)]},MS {toCounts = [(2,4),(3,2)]})]
-}
kSplits :: Ord a => Int -> MS1.Multiset a -> [(MS1.Multiset a, MS1.Multiset a)]
kSplits 0 ms              = [(MS1.emptyMS, ms)]
kSplits _ (MS1.MS [])        = []
kSplits k ms
    | k > MS1.size ms = []
    | otherwise = [(ms1, diff ms1) | ms1 <- MS1.kSubsets k ms]
    where
        ms' = ms1ToMS2 ms
        diff = ms2ToMS1 . MS2.difference ms' . ms1ToMS2


fromPrimeFactors :: MS1.Multiset Integer -> Integer
fromPrimeFactors = product . map (uncurry (^)) . MS1.toCounts

{-
>>> multiplicativePartitions 576
[MS {toCounts = [(3,2),(2,6)]},MS {toCounts = [(3,2),(2,4),(4,1)]},MS {toCounts = [(3,2),(2,3),(8,1)]},MS {toCounts = [(3,2),(2,2),(4,2)]},MS {toCounts = [(3,2),(2,2),(16,1)]},MS {toCounts = [(3,2),(2,1),(4,1),(8,1)]},MS {toCounts = [(3,2),(2,1),(32,1)]},MS {toCounts = [(3,2),(4,3)]},MS {toCounts = [(3,2),(4,1),(16,1)]},MS {toCounts = [(3,2),(8,2)]},MS {toCounts = [(3,2),(64,1)]},MS {toCounts = [(3,1),(2,5),(6,1)]},MS {toCounts = [(3,1),(2,4),(12,1)]},MS {toCounts = [(3,1),(2,3),(6,1),(4,1)]},MS {toCounts = [(3,1),(2,3),(24,1)]},MS {toCounts = [(3,1),(2,2),(6,1),(8,1)]},MS {toCounts = [(3,1),(2,2),(4,1),(12,1)]},MS {toCounts = [(3,1),(2,2),(48,1)]},MS {toCounts = [(3,1),(2,1),(6,1),(4,2)]},MS {toCounts = [(3,1),(2,1),(6,1),(16,1)]},MS {toCounts = [(3,1),(2,1),(4,1),(24,1)]},MS {toCounts = [(3,1),(2,1),(12,1),(8,1)]},MS {toCounts = [(3,1),(2,1),(96,1)]},MS {toCounts = [(3,1),(6,1),(4,1),(8,1)]},MS {toCounts = [(3,1),(6,1),(32,1)]},MS {toCounts = [(3,1),(4,2),(12,1)]},MS {toCounts = [(3,1),(4,1),(48,1)]},MS {toCounts = [(3,1),(12,1),(16,1)]},MS {toCounts = [(3,1),(8,1),(24,1)]},MS {toCounts = [(3,1),(192,1)]},MS {toCounts = [(9,1),(2,6)]},MS {toCounts = [(9,1),(2,4),(4,1)]},MS {toCounts = [(9,1),(2,3),(8,1)]},MS {toCounts = [(9,1),(2,2),(4,2)]},MS {toCounts = [(9,1),(2,2),(16,1)]},MS {toCounts = [(9,1),(2,1),(4,1),(8,1)]},MS {toCounts = [(9,1),(2,1),(32,1)]},MS {toCounts = [(9,1),(4,3)]},MS {toCounts = [(9,1),(4,1),(16,1)]},MS {toCounts = [(9,1),(8,2)]},MS {toCounts = [(9,1),(64,1)]},MS {toCounts = [(2,5),(18,1)]},MS {toCounts = [(2,4),(6,2)]},MS {toCounts = [(2,4),(36,1)]},MS {toCounts = [(2,3),(6,1),(12,1)]},MS {toCounts = [(2,3),(18,1),(4,1)]},MS {toCounts = [(2,3),(72,1)]},MS {toCounts = [(2,2),(6,2),(4,1)]},MS {toCounts = [(2,2),(6,1),(24,1)]},MS {toCounts = [(2,2),(18,1),(8,1)]},MS {toCounts = [(2,2),(4,1),(36,1)]},MS {toCounts = [(2,2),(12,2)]},MS {toCounts = [(2,2),(144,1)]},MS {toCounts = [(2,1),(6,2),(8,1)]},MS {toCounts = [(2,1),(6,1),(4,1),(12,1)]},MS {toCounts = [(2,1),(6,1),(48,1)]},MS {toCounts = [(2,1),(18,1),(4,2)]},MS {toCounts = [(2,1),(18,1),(16,1)]},MS {toCounts = [(2,1),(4,1),(72,1)]},MS {toCounts = [(2,1),(12,1),(24,1)]},MS {toCounts = [(2,1),(36,1),(8,1)]},MS {toCounts = [(2,1),(288,1)]},MS {toCounts = [(6,2),(4,2)]},MS {toCounts = [(6,2),(16,1)]},MS {toCounts = [(6,1),(4,1),(24,1)]},MS {toCounts = [(6,1),(12,1),(8,1)]},MS {toCounts = [(6,1),(96,1)]},MS {toCounts = [(18,1),(4,1),(8,1)]},MS {toCounts = [(18,1),(32,1)]},MS {toCounts = [(4,2),(36,1)]},MS {toCounts = [(4,1),(12,2)]},MS {toCounts = [(4,1),(144,1)]},MS {toCounts = [(12,1),(48,1)]},MS {toCounts = [(36,1),(16,1)]},MS {toCounts = [(8,1),(72,1)]},MS {toCounts = [(24,2)]},MS {toCounts = [(576,1)]}]
-}
multiplicativePartitions :: Integer -> [MS1.Multiset Integer]
multiplicativePartitions = reverse . map (MS1.fromCounts . map (first fromPrimeFactors) . MS1.toCounts) . MS1.partitions . multiplePrimeFactors

{-
>>> take 15 $ numbersWithNFactors 576
[58198140,70450380,78738660,88828740,94954860,96996900,99279180,102965940,106126020,113333220,117417300,120180060,121687020,125585460,126666540]

-}
numbersWithNFactors :: Integer -> [Integer]
numbersWithNFactors n = fromGroups primeGroups mins PQ.empty
    where
        exponents = MS1.fromCounts . map (\(x, c) -> (x - 1, c)) . MS1.toCounts . multiplePrimeFactors $ n
        numPrimes = MS1.size exponents
        primeGroups = kPrimeGroups numPrimes
        exponentPerms = sortBy (flip compare) . MS1.permutations $ exponents
        nFactorNums ps = map (applyExps ps) exponentPerms
        -- Minimum values are those with k-1 True's and then all False's except for a last True, this guarantees the
        mins = tail [applyExps (filterAccordingTo (replicate (numPrimes - 1) True ++ replicate i False ++ [True]) primes) (head exponentPerms) | i <- [0 .. ]]

        fromGroups :: [[[Integer]]] -> [Integer] -> PQ.MinQueue Integer -> [Integer]
        fromGroups (ps:psg) (m:ms) queue = rs ++ fromGroups psg ms queue'
            where
                (rs, queue') = PQ.span (<m) $ foldr PQ.insert queue $ concatMap nFactorNums ps
        fromGroups _ _ _ = []

numFromList :: (Num a) => [a] -> a
numFromList = foldl (\acc n -> (10 * acc) + n) 0

{-
>>> listFromNum 123
[1,2,3]
-}
listFromNum :: (Integral a) => a -> [a]
listFromNum = reverse . listFromNum'
    where
        listFromNum' n
            | d > 0 = m:listFromNum' d
            | otherwise = [m]
            where
                d = div n 10
                m = mod n 10

tabulate :: Ix i => (i -> e) -> (i, i) -> Array i e
tabulate f bounds = array bounds [(x, f x) | x <- range bounds]
