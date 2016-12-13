module Lib where

import Data.List

freq :: Ord a => [a] -> [(a, Int)]
freq = map (\x -> (head x, length x)) . group . sort
