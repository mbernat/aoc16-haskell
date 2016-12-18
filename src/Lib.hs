module Lib where

import Control.Arrow
import Data.List

freq :: Ord a => [a] -> [(a, Int)]
freq = map (head &&& length) . group . sort
