module Day20 where

import Control.Monad
import Data.List
import Data.List.Split
import Data.Ord

type Pair = (Int, Int)

try :: [Pair] -> Int -> Maybe Int
try rules target = do
    prec <- nonEmpty $ filter ((<= target) . fst) rules
    let max = snd $ maximumBy (comparing snd) prec
    guard $ max >= target
    pure $ max + 1
  where
    nonEmpty [] = Nothing
    nonEmpty l = Just l

input0 :: [Pair]
input0 = [(5,8),(0,2),(4,7)]

parse :: String -> Pair
parse line = case splitOn "-" line of
  [x, y] -> (read x, read y)

load = map parse . lines

input = readFile "day20"

solve :: [Pair] -> Int
solve input = last . unfoldr step $ (sortBy (comparing fst) input, 0)
  where
    step (l, t) = case try l t of
      Nothing -> Nothing
      Just t' -> Just (t', (filter ((>t) . fst) l, t'))
