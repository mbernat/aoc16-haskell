{-# LANGUAGE RecordWildCards #-}

module Day4 where

import Data.Char
import Data.List
import Data.List.Split

import Lib


data Room = Room
    { name :: String
    , sector :: Int
    , checksum :: String
    }
  deriving (Show)

input :: IO String
input = readFile "day4"

parse :: String -> Room
parse s = Room name sector (init after)
  where
    [before, after] = splitOn "[" s

    nameSector = splitOn "-" before

    name = concat $ init nameSector

    sector = read $ last nameSector

load :: String -> [Room]
load = map parse . lines

countLex :: (Char, Int) -> (Char, Int) -> Ordering
countLex (a, ac) (b, bc) = if ac == bc then compare a b else compare bc ac

isReal :: Room -> Bool
isReal Room{..} = checksum' == checksum
  where
    checksum' = take 5 . map fst . sortBy countLex $ freq name

solve1 :: [Room] -> Int
solve1 = sum . map sector . filter isReal

shift :: Int -> Char -> Char
shift n c = c'
  where
    c' = chr $ ((ord c - ord 'a' + n ) `mod` 26) + ord 'a'

rotate :: Room -> Room
rotate room@Room{..} = room{name = rotated}
  where
    rotated = map (shift sector) $ name

solve2 :: [Room] -> Maybe Room
solve2 = find (\r -> name r == target) . map rotate . filter isReal
  where
    target = "northpoleobjectstorage"
