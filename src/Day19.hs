module Day19 where

import Data.List
import Data.List.Split

step1 :: [Int] -> [Int]
step1 = map head . fuzzy . chunksOf 2
  where
    fuzzy q = if length (last q) == 1 then last q : init q else q

input :: Int
input = 3014387

solve stepper n = last $ unfoldr step [1..n]
  where
    step l@(x:_:_) = Just (x, stepper l)
    step _ = Nothing

step2Slow :: [Int] -> [Int]
step2Slow xs = take (l - 1) $ drop 1 $ cycle $ a ++ tail b
  where
    l = length xs
    (a, b) = splitAt (l `div` 2) xs
    c = tail b

powerOfThree :: Int -> Int
powerOfThree n = last $ [3^q | q <- [0..20], 3^q < n]

solve2Fast :: Int -> Int
solve2Fast n = if diff <= pot then diff else n * 2 - 3 * pot
  where
    pot = powerOfThree n
    diff = n - pot

{-
 map (solve step2Slow) [1..100]
[1
,1,3
,1,2,3,5,7,9
,1,2,3,4,5,6,7,8,9,11,13,15,17,19,21,23,25,27
,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55,57,59,61,63,65,67,69,71,73,75,77,79,81]
-}
