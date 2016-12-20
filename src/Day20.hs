module Day20 where

import Control.Monad
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Ord

type Pair = (Integer, Integer)

try :: [Pair] -> Integer -> Maybe Integer
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

nextFree :: Integer -> [Pair] -> ([Pair], Integer)
nextFree begin input = last . unfoldr step $ (sortBy (comparing fst) input, begin)
  where
    step (l, t) = case try l t of
      Nothing -> Nothing
      Just t' -> Just ((l', t'), (l', t'))
        where l' = filter ((>t) . fst) l

nextBlock :: [Pair] -> Integer -> Integer
nextBlock input t = fst . minimumBy (comparing fst) $ filter ((>t) . fst) input

-- gaps are in the form [x, y), x, inclusive, y exclusive
nextGap :: (Pair, [Pair]) -> Maybe (Pair, (Pair, [Pair]))
nextGap (_, []) = Nothing
nextGap ((_, prev), rules) = case rest of
    [] -> Nothing
    _ -> Just ((x,y), ((x,y), rest))
  where
    (rest, x) = nextFree prev rules
    y = nextBlock rest x

solve1 = snd . nextFree 0

solve2 :: [Pair] -> Integer
solve2 input = sum . map (\(x, y) -> y - x) $ unfoldr nextGap ((0,0), input)

play input = unfoldr nextGap ((0,0), input)
