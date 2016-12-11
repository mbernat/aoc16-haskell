{-# LANGUAGE LambdaCase #-}
module Day8 where

import Data.List
import qualified Text.Parsec as Parsec
import Text.Parsec hiding (parse)
import Text.Parsec.Char

data Operation
    = Rect Int Int
    | RotateCol Int Int
    | RotateRow Int Int
  deriving (Show)

type Parser = Parsec String ()

rect :: Parser Operation
rect = mkRect
    <$ string "rect "
    <*> many1 digit
    <* string "x"
    <*> many1 digit
  where
    mkRect x y = Rect (read x) (read y)

col :: Parser Operation
col = mkCol
    <$ string "rotate column x="
    <*> many1 digit
    <* string " by "
    <*> many1 digit
  where
    mkCol x y = RotateCol (read x) (read y)

row :: Parser Operation
row = mkRow
    <$ string "rotate row y="
    <*> many1 digit
    <* string " by "
    <*> many1 digit
  where
    mkRow x y = RotateRow (read x) (read y)

op :: Parser Operation
op = try rect <|> try col <|> row

parse :: String -> Operation
parse s = either (error . show) id $ Parsec.parse op "Operation" s

input = readFile "day8"

load = map parse . lines

type Screen = [[Bool]]

mkScreen :: Int -> Int -> Screen
mkScreen x y = replicate x $ replicate y False

applyRect :: Int -> Int -> Screen -> Screen
applyRect a b s = (map applyLine $ take b s) ++ drop b s
  where
    applyLine l = replicate a True ++ drop a l

applyCol a b = transpose . applyRow a b . transpose

applyRow a b s = take a s ++ [rotate (s !! a)] ++ drop (a+1) s
  where
    rotate l = take (length l) . drop (width - b) $ cycle l

    width = length $ s !! 0

applyOp :: Screen -> Operation -> Screen
applyOp s = \case
    Rect a b -> applyRect a b s
    RotateCol a b -> applyCol a b s
    RotateRow a b -> applyRow a b s

printScr :: Screen -> [String]
printScr = map (map (\x -> if x then '#' else '.'))

solve :: [Operation] -> Int
solve = length . filter (== True) . concat . foldl applyOp (mkScreen 6 50)