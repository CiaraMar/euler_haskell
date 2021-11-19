module Euler11 where

import           Control.Arrow
import           Data.Array
import           Data.Ix
import           Lib           (chunks, slidingProduct)

euler11Input :: [Int]
euler11Input = [08, 02, 22, 97, 38, 15, 00, 40, 00, 75, 04, 05, 07, 78, 52, 12, 50, 77, 91, 08,
                49, 49, 99, 40, 17, 81, 18, 57, 60, 87, 17, 40, 98, 43, 69, 48, 04, 56, 62, 00,
                81, 49, 31, 73, 55, 79, 14, 29, 93, 71, 40, 67, 53, 88, 30, 03, 49, 13, 36, 65,
                52, 70, 95, 23, 04, 60, 11, 42, 69, 24, 68, 56, 01, 32, 56, 71, 37, 02, 36, 91,
                22, 31, 16, 71, 51, 67, 63, 89, 41, 92, 36, 54, 22, 40, 40, 28, 66, 33, 13, 80,
                24, 47, 32, 60, 99, 03, 45, 02, 44, 75, 33, 53, 78, 36, 84, 20, 35, 17, 12, 50,
                32, 98, 81, 28, 64, 23, 67, 10, 26, 38, 40, 67, 59, 54, 70, 66, 18, 38, 64, 70,
                67, 26, 20, 68, 02, 62, 12, 20, 95, 63, 94, 39, 63, 08, 40, 91, 66, 49, 94, 21,
                24, 55, 58, 05, 66, 73, 99, 26, 97, 17, 78, 78, 96, 83, 14, 88, 34, 89, 63, 72,
                21, 36, 23, 09, 75, 00, 76, 44, 20, 45, 35, 14, 00, 61, 33, 97, 34, 31, 33, 95,
                78, 17, 53, 28, 22, 75, 31, 67, 15, 94, 03, 80, 04, 62, 16, 14, 09, 53, 56, 92,
                16, 39, 05, 42, 96, 35, 31, 47, 55, 58, 88, 24, 00, 17, 54, 24, 36, 29, 85, 57,
                86, 56, 00, 48, 35, 71, 89, 07, 05, 44, 44, 37, 44, 60, 21, 58, 51, 54, 17, 58,
                19, 80, 81, 68, 05, 94, 47, 69, 28, 73, 92, 13, 86, 52, 17, 77, 04, 89, 55, 40,
                04, 52, 08, 83, 97, 35, 99, 16, 07, 97, 57, 32, 16, 26, 26, 79, 33, 27, 98, 66,
                88, 36, 68, 87, 57, 62, 20, 72, 03, 46, 33, 67, 46, 55, 12, 32, 63, 93, 53, 69,
                04, 42, 16, 73, 38, 25, 39, 11, 24, 94, 72, 18, 08, 46, 29, 32, 40, 62, 76, 36,
                20, 69, 36, 41, 72, 30, 23, 88, 34, 62, 99, 69, 82, 67, 59, 85, 74, 04, 36, 16,
                20, 73, 35, 29, 78, 31, 90, 01, 74, 31, 49, 71, 48, 86, 81, 16, 23, 57, 05, 54,
                01, 70, 54, 71, 83, 51, 54, 69, 16, 92, 33, 48, 61, 43, 52, 01, 89, 19, 67, 48]

{-
>>> euler11Array ! (10, 10)
45
>>> listArray ((1), (27)) $ take 27 euler11Input
array (1,27) [(1,8),(2,2),(3,22),(4,97),(5,38),(6,15),(7,0),(8,40),(9,0),(10,75),(11,4),(12,5),(13,7),(14,78),(15,52),(16,12),(17,50),(18,77),(19,91),(20,8),(21,49),(22,49),(23,99),(24,40),(25,17),(26,81),(27,18)]

>>> range ((1, 20), (20, 20))
[(1,20),(2,20),(3,20),(4,20),(5,20),(6,20),(7,20),(8,20),(9,20),(10,20),(11,20),(12,20),(13,20),(14,20),(15,20),(16,20),(17,20),(18,20),(19,20),(20,20)]

>>> enumFromTo 10 1
[]
-}
euler11Array :: Array (Int, Int) Int
euler11Array = listArray ((1, 1), (20, 20)) euler11Input

{-
>>> walks2D (1, 1) (3, 3)
[[(1,1)],[(1,2),(2,1)],[(1,3),(2,2),(3,1)],[(3,1),(2,2),(1,3)],[(3,2),(2,3)],[(3,3)]]

>>> head $ walks2D (1, 1) (20, 20)
[(1,1)]

>>> take 2 $ (map . map) (euler11Array !) $ walks2D (1, 1) (20, 20)
[[8],[2,49]]
-}
walks2D :: (Ix a) => (a, a) -> (a, a) -> [[(a, a)]]
walks2D (a, b) (a', b') = lefts ++ downs ++ upperRight ++ lowerLeft ++ lowerLeft' ++ upperRight'
    where
        downs = [range ((a, c), (a', c)) | c <-range (b, b')]
        lefts = [range ((c, b), (c, b')) | c <-range (a, a')]
        upperRight = [zip (range (a, a')) (range (c, b')) | c <-range (a, b')]
        lowerLeft = [zip (range (c, a')) (range (b, b')) | c <-range (a, b')]
        -- TODO sort out which of these two is wrong...
        lowerLeft' = [zip (reverse (range (a, a'))) (range (c, b')) | c <-range (a, b')]
        upperRight' = [zip (range (a, a')) (reverse (range (b, c))) | c <-range (a, b')]

{-
>>> take 2 $ concatMap (slidingProduct 4) euler11Walks
[238576,835016]

>>> euler11Walks
[[8,62,13,37,28,84,54,63,78,35,94,55,7,47,35,57,73,36,73,1],[0,36,2,66,20,70,8,96,14,3,58,5,69,99,62,38,41,35,70],[65,36,33,35,66,40,83,0,80,88,44,28,16,20,25,72,29,54],[91,13,17,18,91,14,61,4,24,44,73,7,72,39,30,78,71],[80,12,38,66,88,33,62,0,37,92,97,3,11,23,31,83],[50,64,49,34,97,16,17,44,13,57,46,24,88,90,51],[70,94,89,34,14,54,60,86,32,33,94,34,1,54],[21,63,31,9,24,21,52,16,67,72,62,74,69],[72,33,53,36,58,17,26,46,18,99,31,16],[95,56,29,51,77,26,55,8,69,49,92],[92,85,54,4,79,12,46,82,71,33],[57,17,89,33,32,29,67,48,48],[58,55,27,63,32,59,86,61],[40,98,93,40,85,81,43],[66,53,62,74,16,52],[69,76,4,23,1],[36,36,57,89],[16,5,19],[54,67],[48],[1,73,36,73,57,35,47,7,55,94,35,78,63,54,84,28,37,13,62,8],[70,35,41,38,62,99,69,5,58,3,14,96,8,70,20,66,2,36,0],[54,29,72,25,20,16,28,44,88,80,0,83,40,66,35,33,36,65],[71,78,30,39,72,7,73,44,24,4,61,14,91,18,17,13,91],[83,31,23,11,3,97,92,37,0,62,33,88,66,38,12,80],[51,90,88,24,46,57,13,44,17,16,97,34,49,64,50],[54,1,34,94,33,32,86,60,54,14,34,89,94,70],[69,74,62,72,67,16,52,21,24,9,31,63,21],[16,31,99,18,46,26,17,58,36,53,33,72],[92,49,69,8,55,26,77,51,29,56,95],[33,71,82,46,12,79,4,54,85,92],[48,48,67,29,32,33,89,17,57],[61,86,59,32,63,27,55,58],[43,81,85,40,93,98,40],[52,16,74,62,53,66],[1,23,4,76,69],[89,57,36,36],[19,5,16],[67,54],[48]]

-}
euler11Walks :: [[Int]]
euler11Walks = (map . map) (euler11Array !) $ walks2D (1, 1) (20, 20)

{-
>>> concatMap (slidingProduct 4) $ euler11Walks
[34144,162184,1216380,10500,10920,141960,340704,2433600,2402400,4204200,2802800,9507960,3298680,5452920,991440,1412802,4986360,5355720,5058180,3549600,5797680,2865520,11630640,13956768,569664,741888,666624,8981847,6098785,9832735,4440590,1764070,2982882,2680818,7659480,17696040,10084840,12499520,9374640,419760,388080,57330,68796,1490580,7953400,611800,524400,60720,110880,1912680,765072,4729536,6306048,91392,121856,100352,127232,4707584,294224,189144,242424,774752,1796016,3881712,15284241,19159119,15402429,21149604,12085488,7332768,3934656,1710720,1900800,985600,2956800,2439360,792792,2265120,2165760,8933760,570240,801900,26730,11880,297000,217800,5771700,10231650,4911192,12501216,4717440,2116800,999600,142800,357000,7112448,14224896,3338496,2761472,986240,400660,661960,395200,2647840,6008560,8538480,14942340,14719320,4490640,3160080,2889216,3064320,2369120,70720,168640,101184,29760,1413600,1436400,11251800,21941010,14550354,1847664,786240,1834560,1921920,11771760,27663636,6383916,382800,1052700,1397220,2384910,12401532,18226494,4244526,3344172,10032516,9929088,48477312,8701056,9816576,3476704,3728032,16776144,13725936,156492,558900,3009600,1386000,441000,6638874,3373854,3373854,3304290,1967784,555016,2448600,1432200,3427050,2336625,2928570,283410,338400,90240,59520,317440,55552,124992,106848,373968,2457504,131040,786240,705600,4374720,4895520,2804725,4647830,13193840,6737280,793152,1353024,2129760,5058180,10615920,1548155,221165,137060,67760,358160,3151808,4297920,2051280,3215520,3727080,3354372,2715444,2715444,8372160,2203200,2588760,1502120,1524210,8535576,6628692,12975312,2444624,7508488,5348512,988312,5853848,272272,466004,1507660,783200,138112,3349216,2254280,27896715,5377680,388080,1075536,619248,1238496,2830848,758784,346112,854464,1762332,1830114,6898122,5762988,18741888,12139632,20907144,6149160,5088960,267840,198720,327888,305118,4678476,5593830,2034120,971520,1330560,2249856,9936864,21426363,196224,1864128,1109600,2704650,407550,257400,967824,1786752,2923776,974592,476928,192096,341504,1707520,2301440,6031360,6785280,2036880,7332768,3188160,2036880,4371840,2064480,4266592,18364896,14399748,34728804,37529514,22366074,27552410,24864370,1484440,905760,170496,1481900,5779410,2454270,6310980,217620,206460,206460,112406,7980826,5176752,14361312,23740128,5349888,2563488,1699056,104880,353970,268380,22275540,16229322,16229322,15772158,3040416,5484672,3351744,2331648,8889408,4154832,6547008,136396,199004,87932,113297,5438256,1651104,4540536,2223936,878592,1132032,1234944,1080576,2633904,628992,2253888,2039232,104576,575168,26752,28160,140800,1600,336140,5210170,4997510,9995020,3712436,6586580,5045040,875160,1312740,1336608,2970240,9085440,8386560,6289920,5425056,7615944,14808780,6414210,4664880,1507840,3939840,829440,3006720,2161080,1414040,353510,705024,313344,1370880,1088640,6514520,4768360,7152540,2743440,8111040,571200,85680,85680,52920,508032,3838464,11378304,23569344,35845044,21612453,7551339,6162587,142120,190740,1110780,1292544,646272,836352,633600,217800,10454400,5544000,369600,1629600,967575,1050510,15127344,12164256,17712864,5759100,25723980,952740,277380,286626,312294,17519250,8175650,14482580,5099500,1627500,1441500,1185750,174636,436590,2089395,2279340,3581820,6049296,2798928,7230564,6500204,4019863,12837627,8282340,3629340,1776060,1614600,4359420,2777040,6178914,216804,74760,35600,10400,228800,1532960,3602456,969892,1520967,363216,556416,874368,1115136,69696,66792,15785820,11576268,3236376,4455880,10541960,4791800,2764500,1600500,82500,115500,53900,2940,14112,17136,181152,966144,11118600,13638816,11757600,6292800,16518600,3052350,1831410,4530330,4170780,10794960,17511824,18070712,14331944,30618244,26004536,8310728,16621456,184960,1664640,3231360,3231360,4466880,9678240,10264800,769860,720720,406560,1068672,20304768,7614288,12459744,13407768,11525976,11525976,750400,8104320,10738224,10738224,7478406,10802142,2853396,3407040,2096640,994560,923520,369408,1031264,501696,2662848,5908194,4232736,36358,114268,90948,101244,6378372,27832896,2785024,506368,482816,1448448,1920768,9444864,4843520,4055040,2488320,622080,1290816,2187216,2511248,5336402,3857640,3288480,1379040,4461600,3420560,4407260,14577860,11397236,6027840,4636800,5644800,13171200,9408000,3292800,1293600,295680,399168,598752,308448,501228,111384,153816,533832,1663092,7206732,122688,286272,119280,2624160,3363360,10570560,51267216,10874864,2868096,1890336,1500576,8467536,11290048,6228992,6876160,1392640,43520,362600,478632,4188030,1538460,2744280,1413720,1373328,686664,374544,561816,66096,242352,424116,332640,6153840,4289040,6059120,112112,48048,14586,42636,1044582,2817206,5137258,7165123,4240583,2572938,7386822,3763098,12067866,13855698,622728,1314648,268584,7312032,1044576,202176,359424,938496,4548096,12507264,10943856,9896040,2670360,4450600,7788550,4856390,21710920,14210784,725040,916560,23660000,25480000,5880000,5292000,10054800,13214880,35868960,28894440,12166080,8727840,10565280,6557760,2623104,2146176,1492992,279496,1781787,109089,235773,205020,389940,5849100,261900,314280,142560,164736,1427712,1903616,1730560,133120,25600,38400,57816,1936836,880380,120600,2864250,726750,565250,4522000,5076540,5245758,11108664,7593264,2904000,8316000,415800,196560,206388,255528,1788696,275184,74256,19992,109956,907137,4962573,17959788,3731904,1432981,1314797,3403004,1636888,13987952,12259104,776736,338256,331992,206064,1809864,1952748,1808100,5166000,4797000,11232000,9135360,3654144,2248704,1194624,1742976,10675728,30692718,540270,3313656,6075036,14035428,12814956,11561319,11561319,2415798,1380456,898128,1496880,7056720,4580064,4258656,2701728,900576,350224,640976,97776,354438,430389,177480,11587200,15642720,20335536,15466464,9097920,14826240,6462720,6343040,13478960,8883860,4287360,1774080,3104640,5045040,7796880,6713980,5371184,3364368,85425,45560,225120,221760,14636160,32565456,12793572,17833464,271360,2713600,1356800,460800,705600,1111320,5278770,2414720,13522432,4829440,2085440,3500560,9001440,641130,6044940,2389860,5098368,1507968,597402,252747,43956,1025640,244608,61152,63700,22464,149760,279496,1781787,109089,235773,205020,389940,5849100,261900,314280,142560,164736,1427712,1903616,1730560,133120,25600,38400,16194745,32719995,15358365,1940004,710424,143520,586560,4301440,6121280,26321504,7280416,992784,858624,738816,1619712,9043392,5443200,4300800,3809280,23569920,17284608,4051080,3789720,1684320,497640,530816,503360,331760,2169200,3118225,1077205,1444352,55552,130816,310688,743432,20444380,12322640,14916880,7124480,5958656,6229504,3995008,1997504,3864736,5695272,17085816,531805,977835,1393935,2230296,2046984,2908872,43416,235200,735000,562500,871875,1220625,455700,589372,627396,1613304,4724676,4188888,6600672,434304,298584,401940,616770,4728570,1504545,1977402,1599696,1599696,10969344,10253952,2373140,3400320,4389504,8969856,5125632,160176,212064,446688,1982178,40304286,1923264,2804760,7323540,13679820,23451120,16080768,10606464,5249664,3499776,85680,20400,42000,168000,38500,261800,231880,382602,15949128,20640048,15746016,23365056,6023808,21459816,9580275,2720325,32775,9200,4788480,2115840,793440,8926200,7079400,4904432,18585216,11079648,8798544,94464,1842048,2609568,3858624,3639384,191844,10935108,5242860,6844845,656355,633325,1700930,1266650,14114100,16167060,9287460,22289904,8001504,4699296,1131312,835016,238576,3817100,3380860,9563004,16093836,2117610,1980990,60030,12180,233856,32256,752640,1075200,739200,184800,95040,2818800,1044000,576000,224000,394240,1734656,8673280,7669200,3049200,2744280,2702700,6479460,6570720,589680,1434888,1618848,539616,308352,257664,81984,310856,1398852,389844,361998,361998,650969,23529,73623,294492,990564,11883168,7283232,2648448,2407680,9694080,8743680,5537664,818064,1499784,554268,155584,1160896,897056,2585632,10342528,5331200,172584,105468,3374976,8536704,5448960,8916480,3900960,1542240,2287656,3982216,19911080,22793184,22132512,4785408,4013568,1170624,419328,235872,140616,421848,369117,883872,2541132,2131272,365976,1179256,922896,1881288,3651912,4533408,2488416,1487640,789360,880880,5615610,2960958,6377448,7868280,8837796,3213744,3575856,174432,204768,1450440,1689120,4476672,2984448,2051808,2725536,1597728,2845953,9904448,10229184,3211488,2993760,5426190,11842200,25612200,30987600,14582400,3817216,3890624,16048824,6992,482448,3298400,1909600,1768425,3654745,5159640,1639440,951280,553472,227520,403848,717952,890624,14917952,2441880,2570400,14871600,6444360,3032640,1435500,1282380,2115927,2042964,3714480,3049200,646800,1359288,21408786,738234,508032,822528,757248,9086976,11074752,416415,7412187,19620495,6355935,184230,10350,5290,3910,31280,268800,1574400,78720,131856,340628,548328,2467476,1951884,1227798,1599858,34826064,41076896,18442688,2752640,485760,385440,657000,1839600,766500,588000,148960,6636448,4594464,8013600,3931200,1404000,3861000,72864,327888,156816,67716,6433020,3097380,7133360,14079000,14227200,26265600,27993600,19408896,17791488,2304000,1013760,1119360,1492480,2938320,12955320,10755360,8335404,4630780,1670900,2582300,666400,685440,78336,51744,26880,524160,11706240,19649760,8351148,2141320,2141320,706180,2949340,737335,913415,2003620,1185240,4740960,17728984,7792960,5009760,6032160,3313440,6461208,8076510,2053350,2474550,2823525,5898030,38140594,70600674,12692256,9316656,1920960,238576,835016,1131312,4699296,8001504,22289904,9287460,16167060,14114100,1266650,1700930,633325,656355,6844845,5242860,10935108,191844]

>>> euler11
70600674
-}
euler11 :: Int
euler11 = maximum . concatMap (slidingProduct 4) $ euler11Walks


senses :: [(Int, Int) -> (Int, Int)]
senses = [(+1) *** id,(+1) *** (+1), id *** (+1), (+1) *** (\n -> n - 1)]

inArray a i = inRange (bounds a) i

prods :: Array (Int, Int) Int -> [(Int, [Int])]
prods a = [(product xs, xs) | i <- range $ bounds a,
                        s <- senses,
                        let is = take 4 $ iterate s i,
                        all (inArray a) is,
                        let xs = map (a!) is]

{-
>>> euler11'
(70600674,[89,94,97,87])
-}
euler11' = maximum . prods $ euler11Array
