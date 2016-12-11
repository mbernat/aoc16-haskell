module Day3 where

import Data.List
import Data.List.Split


input :: IO String
input = readFile "day3"

input2 :: IO String
input2 = readFile "day3-small"

load :: String -> [[Int]]
load = map (map read . words) . lines

triangle :: [Int] -> Bool
triangle [a,b,c] = a + b > c && b + c > a && c + a > b

solve1 :: [[Int]] -> Int
solve1 = length . filter triangle

solve2 :: [[Int]] -> Int
solve2 = sum . map (length . filter triangle . chunksOf 3) . transpose
