module Lib where

import Data.List (foldl')
import qualified Data.Map.Strict as Map

freq :: Ord a => [a] -> [(a, Int)]
freq = Map.toList . foldl' f Map.empty where
    f m k = Map.insertWith (+) k 1 m
