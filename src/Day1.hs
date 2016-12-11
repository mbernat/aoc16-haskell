module Day1 where

import Data.List
import Data.List.Split

import Lib


data Turn = L | R
data Move = Move Turn Int

parse :: String -> Move
parse ('R':s) = Move R (read s)
parse ('L':s) = Move L (read s)

items :: String -> [String]
items = filter (/= "") . splitOneOf ", "

type Location = (Int, Int)

data Direction = North | East | West | South

data State = State
    { pos :: Location
    , dir :: Direction
    }

turn :: Direction -> Turn -> Direction
turn North L = West
turn West L = South
turn South L = East
turn East L = North
turn North R = East
turn East R = South
turn South R = West
turn West R = North

walk :: State -> Int -> State
walk (State (x, y) dir) s = State (x', y') dir
  where
    (x', y') = case dir of
        North -> (x, y + s)
        East -> (x + s, y)
        South -> (x, y - s)
        West -> (x - s, y)

move :: State -> Move -> State
move (State pos dir) m@(Move t s) = newState
  where
    newState = walk turnedState s
    turnedState = State pos (turn dir t)

visit :: State -> Move -> [State]
visit (State pos dir) m@(Move t s) = states
  where
    states = map (walk turnedState) [1..s]
    turnedState = State pos (turn dir t)

dist :: Location -> Int
dist (x, y) = abs x + abs y

initialState = State { pos = (0,0), dir = North }

load :: String -> [Move]
load = map parse . items

solve1 :: String -> Int
solve1 = dist . pos . foldl move initialState . load

visited :: [Move] -> [Location]
visited = map pos . concat . scanl (visit . last) [initialState]

repeated :: [Location] -> [Location]
repeated = map fst . filter (\(x, c) -> c > 1) . freq

solve2 inp = find (`elem` multiple) locs
  where
    multiple = repeated locs
    locs = visited $ load inp






input = "L5, R1, R4, L5, L4, R3, R1, L1, R4, R5, L1, L3, R4, L2, L4, R2, L4, L1, R3, R1, R1, L1, R1, L5, R5, R2, L5, R2, R1, L2, L4, L4, R191, R2, R5, R1, L1, L2, R5, L2, L3, R4, L1, L1, R1, R50, L1, R1, R76, R5, R4, R2, L5, L3, L5, R2, R1, L1, R2, L3, R4, R2, L1, L1, R4, L1, L1, R185, R1, L5, L4, L5, L3, R2, R3, R1, L5, R1, L3, L2, L2, R5, L1, L1, L3, R1, R4, L2, L1, L1, L3, L4, R5, L2, R3, R5, R1, L4, R5, L3, R3, R3, R1, R1, R5, R2, L2, R5, L5, L4, R4, R3, R5, R1, L3, R1, L2, L2, R3, R4, L1, R4, L1, R4, R3, L1, L4, L1, L5, L2, R2, L1, R1, L5, L3, R4, L1, R5, L5, L5, L1, L3, R1, R5, L2, L4, L5, L1, L1, L2, R5, R5, L4, R3, L2, L1, L3, L4, L5, L5, L2, R4, R3, L5, R4, R2, R1, L5"

