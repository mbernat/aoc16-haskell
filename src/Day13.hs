{-# LANGUAGE RecordWildCards #-}
module Day13 where

import Data.Bits
import Data.Dequeue
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

type Pos = (Int, Int)

input0 = 10
input1 = 1358
input2 = 1362

expr :: Pos -> Int
expr (x, y) = x*x + 3*x + 2*x*y + y + y*y

isOpen :: Pos -> Bool
isOpen p = even . popCount $ expr p + input1

valid :: Pos -> Bool
valid p@(x,y) = x >= 0 && y >= 0 && isOpen p

dirs :: [Pos]
dirs = [(0,1), (0,-1), (1,0), (-1,0)]

neighbours :: Pos -> [Pos]
neighbours (x,y) = filter valid $ map (\(dx, dy) -> (x + dx, y + dy)) dirs

data State = State
  { visited :: Set (Pos, Int)
  , next :: BankersDequeue (Pos, Int)
  }

initialState = State Set.empty (pushBack empty ((1,1), 0))

walk :: Pos -> State -> State
walk dest s@State{..} = if current == dest then s else State visited' next'
  where
    ((current, dist), rest) = fromJust $ popFront next
    visited' = Set.insert (current, dist) visited
    unvisNeigh = map (\p -> (p, dist + 1)) .
      filter (\p -> not $ p `elem` (Set.map fst visited)) $ neighbours current
    next' = foldl pushBack rest unvisNeigh

dest0 :: (Int, Int)
dest0 = (7, 4)

dest1 :: (Int, Int)
dest1 = (31, 39)

solve :: Pos -> [State]
solve dest = iterate (walk dest) initialState
