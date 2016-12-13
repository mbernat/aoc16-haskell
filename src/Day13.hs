{-# LANGUAGE RecordWildCards #-}
module Day13 where

import Data.Bits
import Data.Dequeue
import Data.Maybe

type Pos = (Int, Int)

input0 = 10
input1 = 1358

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
  { visited :: [Pos]
  , next :: BankersDequeue (Pos, Int)
  }

initialState = State [] (pushBack empty ((1,1), 0))

walk :: Pos -> State -> State
walk dest State{..} =
 State visited' next'
  where
    ((current, dist), rest) = fromJust $ popFront next
    visited' = current : visited
    unvisNeigh = map (\p -> (p, dist + 1)) .
      filter (\p -> not $ p `elem` visited) $ neighbours current
    next' = if current == foldl pushBack rest unvisNeigh

dest0 :: (Int, Int)
dest0 = (7, 4)

dest1 :: (Int, Int)
dest1 = (31, 39)

solve :: Pos -> [State]
solve dest = iterate (walk dest) initialState
