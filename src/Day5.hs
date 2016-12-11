{-# LANGUAGE OverloadedStrings #-}

module Day5 where

import Control.Parallel.Strategies
import Data.Hash.MD5
import Data.List
import Data.List.Split


mkHash :: String -> Int -> String
mkHash s n = md5s . Str $ s ++ show n

hashes :: String -> [Int] -> [(Int, String)]
hashes input l = filter (zeroed . snd) $ map hash l
  where
    hash n = (n, mkHash input n)

    zeroed = (== "00000") . take 5

solve :: String -> [(Int, String)]
solve input = runEval $ do
    res <- mapM (rpar . hashes input) $ chunksOf part [a..(a+n)]
    res' <- mapM rseq res
    pure . sort . concat $ res'

  where
    a = 10000000
    n = 10000000
    part = n `div` 4

{-
Got too lazy, processed the md5s manually... :'(

863dde27

77
3d
5e
4d
35
51
08
23
00
16
56
0e
62
-}