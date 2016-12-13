{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Day12 where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Text.Parsec hiding (State)

data Reg = A | B | C | D
  deriving (Eq, Ord, Show)

data Number = Reg Reg | Value Int
  deriving (Show)

data Instr
  = Copy Number Reg
  | Inc Reg
  | Dec Reg
  | Jump Number Int
  deriving (Show)

type Parser = Parsec String ()

reg :: Parser Reg
reg =
    (try (char 'a') >> pure A) <|>
    (try (char 'b') >> pure B) <|>
    (try (char 'c') >> pure C) <|>
    (try (char 'd') >> pure D)

nat :: Parser Int
nat = read <$> many1 digit

int :: Parser Int
int = mkInt <$> (option 1 (char '-' >> pure (-1))) <*> nat
  where
    mkInt sign n = sign * n

copy :: Parser Instr
copy = Copy
  <$ string "cpy "
  <*> number
  <* space
  <*> reg

inc :: Parser Instr
inc = Inc <$ string "inc " <*> reg

dec :: Parser Instr
dec = Dec <$ string "dec " <*> reg

number :: Parser Number
number = try (Reg <$> reg) <|> (Value <$> int)

jump :: Parser Instr
jump = Jump
  <$ string "jnz "
  <*> number
  <* space
  <*> int

instr :: Parser Instr
instr = try copy <|> try inc <|> try dec <|> try jump

parse' s = either (error . show) id $ parse instr "Instruction" s

input = readFile "day12"

load = map parse' . lines

initialRegs1 :: Map Reg Int
initialRegs1 = Map.fromList [(A, 0), (B, 0), (C, 0), (D, 0)]

initialRegs2 :: Map Reg Int
initialRegs2 = Map.fromList [(A, 0), (B, 0), (C, 1), (D, 0)]

data State = State
    { regs :: Map Reg Int
    , instrs :: [Instr]
    , pos :: Int
    }
  deriving (Show)

step :: State -> State
step state@State{..} =
  if pos >= length instrs then state
  else case (instrs !! pos) of
    Copy from to -> state{regs = Map.insert to val regs, pos = pos + 1}
      where
        val = case from of
          Reg reg -> fromJust $ Map.lookup reg regs
          Value n -> n
    Inc reg -> state{regs = Map.adjust (+ 1) reg regs, pos = pos + 1}
    Dec reg -> state{regs = Map.adjust (\x -> x - 1) reg regs, pos = pos + 1}
    Jump test jumpPos -> state{pos = pos'}
      where
        pos' = if val /= 0 then pos + jumpPos else pos + 1
        val = case test of
          Reg reg -> fromJust $ Map.lookup reg regs
          Value n -> n

solve1 :: [Instr] -> [State]
solve1 instrs = iterate step (State initialRegs1 instrs 0)

solve2 :: [Instr] -> [State]
solve2 instrs = iterate step (State initialRegs2 instrs 0)
