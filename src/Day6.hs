module Day6 where

import Data.List
import Data.Ord

import Lib


input = readFile "day6"
load = lines

solve :: [String] -> String
solve = map (fst . head . sortBy (comparing $ Down . snd) .  freq) . transpose

solve2 :: [String] -> String
solve2 = map (fst . head . sortBy (comparing snd) .  freq) . transpose
