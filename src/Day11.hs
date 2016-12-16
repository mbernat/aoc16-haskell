{-# LANGUAGE RecordWildCards #-}
module Day11 where

import Data.Dequeue
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set

data Item = Ge | Mi
  deriving (Eq, Ord, Show)

data TutMaterial = H | L
  deriving (Eq, Ord, Show)

data FirstMaterial = R | P
  deriving (Eq, Ord, Show)

type Object a = (a, Item)

type Objects a = Map.Map Int (Set.Set (Object a))

data State a = State
    { el :: Int
    , obj :: Objects a
    }
  deriving (Eq, Ord, Show)

initial0 :: State TutMaterial
initial0 = State 0 $ Map.fromList
    [ (0, Set.fromList [(H, Mi), (L, Mi)])
    , (1, Set.fromList [(H, Ge)])
    , (2, Set.fromList [(L, Ge)])
    , (3, Set.empty)
    ]

subsets :: Ord a => Set.Set a -> [Set.Set a]
subsets s = List.nub $ map Set.fromList $ [[e1, e2] | e1 <- e, e2 <- e]
  where
    e = Set.elems s

lookup k = fromJust . Map.lookup k

move :: Ord a => State a -> Int -> Set.Set (Object a) -> State a
move State{..} to what = State{el = to, obj = obj''}
  where
    obj' = Map.adjust (`Set.difference` what) el obj
    obj'' = Map.adjust (`Set.union` what) to obj'

moves :: Ord a => State a -> [State a]
moves s@State{..} =
    [move s e what | e <- Map.keys obj, e /= el, what <- toMove el]
  where
    toMove level = subsets $ fromJust $ Map.lookup level obj

valid :: Ord a => State a -> Bool
valid = List.all (shielded . Set.toList) . Map.elems . obj
  where
    shielded items =
        (Set.null $ Set.difference (gens items) (chips items)) ||
        (Set.null $ Set.difference (chips items) (gens items))
    chips = Set.fromList . List.map fst . List.filter ((== Mi) . snd)
    gens = Set.fromList . List.map fst . List.filter ((== Ge) . snd)

neighbours = filter valid . moves

data Tr a = Tr
    { visited :: Set.Set (State a, Int)
    , next :: BankersDequeue (State a, Int)
    }
  deriving (Show)

initialTr0 = Tr Set.empty (pushBack empty (initial0, 0))

destObj0 = Map.fromList
    [ (0, Set.empty)
    , (1, Set.empty)
    , (2, Set.empty)
    , (3, Set.fromList [(H, Ge), (H, Mi), (L, Ge), (L, Mi)])
    ]

walk :: Ord a => (State a -> [State a]) -> State a -> Tr a -> Maybe (Tr a, Tr a)
walk neigh dest s@Tr{..} = nextTr
  where
    ((current, dist), rest) = fromJust $ popFront next
    visited' = Set.insert (current, dist) visited
    unvisNeigh = map (\p -> (p, dist + 1)) .
      filter (\p -> not $ p `elem` (Set.map fst visited)) $ neigh current
    next' = foldl pushBack rest unvisNeigh
    nextTr = if current == dest then Nothing else Just $ (s, Tr visited' next')

solve = List.unfoldr (walk neighbours (State 3 destObj0)) initialTr0

active :: Tr a -> (State a, Int)
active = fst . fromJust . popFront . next
