{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Day11 where

import Control.Monad
import qualified Data.PQueue.Min as PQueue
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe
import Data.Ord
import qualified Data.Set as Set

data Item = Ge | Mi
  deriving (Eq, Ord, Show)

data TutMaterial = H | L
  deriving (Eq, Ord, Show)

data FirstMaterial = Pr | Co | Cu | Ru | Pu
  deriving (Eq, Ord, Show)

type Object a = (a, Item)

type Objects a = Map.Map Int (Set.Set (Object a))

data State a = State
    { el :: Int
    , steps :: Int
    , obj :: Objects a
    }
  deriving (Show)

instance Eq a => Eq (State a) where
    s1 == s2 = (el s1, obj s1) == (el s2, obj s2)

instance Ord a => Ord (State a) where
    compare s1 s2 = compare (el s1, obj s1) (el s2, obj s2)

newtype DistState a = DistState { fromDS :: (State a) }
  deriving (Eq, Show)

-- this assumes that the final state is everything on the third floor
distance :: State a -> Int
distance State{..} = (objDist `div` 2) + steps
  where
    objDist = sum $ Map.mapWithKey (\k v -> (3 - k) * Set.size v) obj

instance Ord a => Ord (DistState a) where
    compare = comparing $ distance . fromDS

initial0 :: State TutMaterial
initial0 = State 0 0 $ Map.fromList
    [ (0, Set.fromList [(H, Mi), (L, Mi)])
    , (1, Set.fromList [(H, Ge)])
    , (2, Set.fromList [(L, Ge)])
    , (3, Set.empty)
    ]

initial1 :: State FirstMaterial
initial1 = State 0 0 $ Map.fromList
    [ (0, Set.fromList [(Pr, Ge), (Pr, Mi)])
    , (1, Set.fromList [(Co, Ge), (Cu, Ge), (Ru, Ge), (Pu, Ge)])
    , (2, Set.fromList [(Co, Mi), (Cu, Mi), (Ru, Mi), (Pu, Mi)])
    , (3, Set.empty)
    ]

subsets :: Ord a => Set.Set a -> [Set.Set a]
subsets s = List.nub $ map Set.fromList $ [[e1, e2] | e1 <- e, e2 <- e]
  where
    e = Set.elems s

see k = fromJust . Map.lookup k

moves :: Ord a => State a -> [State a]
moves s@State{..} = do
    let stuffAtEl = see el obj
    toMove <- subsets stuffAtEl
    let lessStuffAtEl = Set.difference stuffAtEl toMove
    guard $ valid lessStuffAtEl
    let obj' = Map.insert el lessStuffAtEl obj
    to <- [t | t <- [el + 1, el - 1], t >= 0, t < 4]
    let moreStuffAtTo = Set.union toMove $ see to obj
    guard $ valid moreStuffAtTo
    let obj'' = Map.insert to moreStuffAtTo obj'
    pure $ State{el = to, obj = obj'', steps = steps + 1}

valid :: Ord a => Set.Set (Object a) -> Bool
valid = shielded . Set.toList
  where
    shielded items =
        (Set.null $ (gens items)) ||
        (Set.null $ Set.difference (chips items) (gens items))
    chips = Set.fromList . List.map fst . List.filter ((== Mi) . snd)
    gens = Set.fromList . List.map fst . List.filter ((== Ge) . snd)

data Tr a = Tr
    { visited :: Set.Set (State a)
    , next :: PQueue.MinQueue (DistState a)
    }
  deriving (Show)

mkInitialTr :: Ord a => State a -> Tr a
mkInitialTr s = Tr Set.empty (PQueue.insert (DistState s) PQueue.empty)

allObjects :: Ord a => State a -> Set.Set (Object a)
allObjects s = foldl Set.union Set.empty (Map.elems $ obj s)

mkDest :: Ord a => State a -> State a
mkDest s = State 3 0 $ Map.fromList
    [ (0, Set.empty)
    , (1, Set.empty)
    , (2, Set.empty)
    , (3, allObjects s)
    ]

walk :: Ord a =>
    (State a -> [State a]) -> State a -> Tr a -> Maybe (Tr a, Tr a)
walk neigh dest s@Tr{..} = nextTr
  where
    (currentDS, rest) = PQueue.deleteFindMin next
    current = fromDS currentDS
    visited' = Set.insert current visited
    unvisNeigh = filter (\p -> not $ p `elem` visited) $ neigh current
    next' = foldl (\q n -> PQueue.insert (DistState n) q) rest unvisNeigh
    nextTr = if current == dest then Nothing
             else Just $ (Tr visited' next', Tr visited' next')

solve0 = List.unfoldr (walk moves (mkDest initial0)) (mkInitialTr initial0)

solve1 = List.unfoldr (walk moves (mkDest initial1)) (mkInitialTr initial1)

active :: Tr a -> State a
active = fromDS . PQueue.findMin . next

