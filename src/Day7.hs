{-# LANGUAGE RecordWildCards #-}
module Day7 where

import Data.List
import Data.List.Split


data IP = IP
    { supernet :: [String]
    , hypernet :: [String]
    }

input :: IO String
input = readFile "day7"

parse :: String -> IP
parse s = IP (map head parts) (concatMap tail parts)
  where
    parts = chunksOf 2 $ splitOneOf "[]" s

load :: String -> [IP]
load = map parse . lines


isAbba :: String -> Bool
isAbba [a, b, c, d] = a == d && b == c && a /= b

hasAbba :: String -> Bool
hasAbba = any isAbba . map (take 4) . drop 4 . reverse . tails

hasTls :: IP -> Bool
hasTls IP{..} = any hasAbba supernet && not (any hasAbba hypernet)

solve1 :: [IP] -> Int
solve1 = length . filter hasTls


isAba :: String -> Bool
isAba [a, b, c] = a == c && a /= b

findAbas :: String -> [String]
findAbas = filter isAba . map (take 3) . drop 3 . reverse . tails

toBab :: String -> String
toBab [a, b, _] = [b, a, b]

hasSsl :: IP -> Bool
hasSsl IP{..} = not . null $ intersect abas babs
  where
    abas = concatMap findAbas supernet
    babs = concatMap (map toBab . findAbas) hypernet

solve2 :: [IP] -> Int
solve2 = length . filter hasSsl
