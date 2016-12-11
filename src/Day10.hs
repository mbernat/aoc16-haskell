{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Day10 where

import qualified Data.List as List
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import Text.Parsec hiding (State)

type Parser = Parsec String ()

data Destination = Bot Int | Output Int
  deriving (Show)

data Compare = Compare
    { low :: Destination
    , high :: Destination
    }
  deriving (Show)

data Instruction = Value Int Int | Edges Int Compare
  deriving (Show)

bot :: Parser Int
bot = read  <$ string "bot " <*> many1 digit

value :: Parser Int
value = read  <$ string "value " <*> many1 digit

destBot :: Parser Destination
destBot = Bot <$> bot

output :: Parser Destination
output = (Output . read)  <$ string "output " <*> many1 digit

destination = try destBot <|> output

edges :: Parser Instruction
edges = mkCompare
    <$> bot
    <* string " gives low to "
    <*> destination
    <* string " and high to "
    <*> destination
  where
    mkCompare from lo hi = Edges from (Compare lo hi)

fromDest :: Destination -> Int
fromDest = \case
    Bot x -> x
    Output x -> x

valueGoes :: Parser Instruction
valueGoes = Value
    <$> value
    <* string " goes to "
    <*> bot

instruction :: Parser Instruction
instruction = try edges <|> valueGoes

parse' s = either (error . show) id $ parse instruction "Inst" s

input = readFile "day10"

load = map parse' . lines

type Graph = Map Int Compare

type Values = Map Int [Int]

type Outputs = Map Int Int

data State = State
    { graph :: Graph
    , values :: Values
    , outputs :: Outputs
    }

addValue :: Int -> Maybe [Int] -> Maybe [Int]
addValue v Nothing = Just [v]
addValue v (Just vs) = Just (v : vs)

addInstruction :: (Graph, Values) -> Instruction -> (Graph, Values)
addInstruction (g, vs) = \case
    Value v b -> (g, Map.alter (addValue v) b vs)
    Edges b c -> (Map.insert b c g, vs)

mkGraphAndValues :: [Instruction] -> (Graph, Values)
mkGraphAndValues = foldl addInstruction (Map.empty, Map.empty)

addDest :: Int -> Destination -> State -> State
addDest val dest state@State{..} = case dest of
    Bot b -> state{values = Map.alter (addValue val) b values}
    Output o -> state{outputs = Map.insert o val outputs}

findActive :: Values -> [(Int, [Int])]
findActive values =
    Map.toList $ Map.filter (\vs -> length vs > 1) values

stepOne :: State -> (Int, [Int]) -> State
stepOne state@State{..} (active, vals) = state'''
  where
    comp = fromJust $ Map.lookup active graph

    [lo, hi] = List.sort vals

    state' = state{values=Map.delete active values}

    state'' = addDest lo (low comp) state'

    state''' = addDest hi (high comp) state''

step :: State -> State
step s = foldl stepOne s $ active
  where
    active = findActive (values s)

run :: [Instruction] -> [State]
run insts = iterate step (State g vs Map.empty)
  where
    (g, vs) = mkGraphAndValues insts

solve :: [Instruction] -> Maybe State
solve = List.find target . run
  where
    target State{..} = any (== [17, 61]) . map snd $ findActive values

