module Day14 where

import Data.Hash.MD5
import Data.List
import qualified Data.Map as Map
import Data.Maybe

input0 = "abc"
input1 = "yjdafjpo"

sHash :: String -> String
sHash s = md5s $ Str s

mkHash :: String -> Int -> String
mkHash s n = sHash $ s ++ show n

stretch :: String -> String
stretch = (!! 2016) . iterate sHash

triple :: String -> Maybe Char
triple = (head <$>) . find ((>= 3) . length) . group

quint :: String -> [Char]
quint = map head . filter ((>= 5) . length) . group

type Cache = Map.Map Char [Int]

emptyCache :: Cache
emptyCache = Map.fromList $ zip (['a'..'f'] ++ ['0'..'9']) (repeat [])

populateCache :: String -> Int -> Cache
populateCache input up = foldl add emptyCache [0..up]
  where
    add m n = foldl (addQuint n) m (quint $ mkHash input n)
    addQuint n m c = Map.update (Just . (n:)) c m

populateCache2 :: [String] -> Cache
populateCache2 hashes = foldl add emptyCache (zip [0..] hashes)
  where
    add m (n, hash) = foldl (addQuint n) m (quint hash)
    addQuint n m c = Map.update (Just . (n:)) c m


isKey :: String -> Cache -> Int -> Bool
isKey input cache n = case triple hash of
    Nothing -> False
    Just c -> any (\x -> x > n && x <= n + 1000)
              $ fromJust $ Map.lookup c cache
  where
    hash = mkHash input n

isKey2 :: Cache -> (Int, String) -> Bool
isKey2 cache (n, hash) = case triple hash of
    Nothing -> False
    Just c -> any (\x -> x > n && x <= n + 1000)
              $ fromJust $ Map.lookup c cache

solve input = filter (isKey input cache) [0..q] !! 63
  where
    q = 30000
    cache = populateCache input (q + 1000)

hashes :: IO [String]
hashes = lines <$> readFile "hashes"

solve2 :: [String] -> Int
solve2 hashes = fst . (!! 63) $ filter (isKey2 cache) zipped
  where
    zipped = zip [0..] hashes
    cache = populateCache2 hashes

