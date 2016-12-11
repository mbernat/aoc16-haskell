module Day9 where

import qualified Text.Parsec as Parsec
import Text.Parsec hiding (parse)
import Text.Parsec.Char
import Lib


data Code = Raw String | Repeat Int String
  deriving (Show)

type Parser = Parsec String ()


input = readFile "day9"

marker :: Parser Code
marker = do
    string "("
    subseqCount <- read <$> many1 digit
    string "x"
    times <- read <$> many1 digit
    string ")"
    subseq <- count subseqCount anyChar
    pure $ Repeat times subseq

raw :: Parser Code
raw = Raw <$> (many1 $ noneOf "(")

code :: Parser Code
code = try marker <|> raw

parse :: String -> [Code]
parse s = either (error . show) id $ Parsec.parse (many code) "Codes" s

solve :: [Code] -> String
solve = concatMap runCode
  where
    runCode (Raw s) = s
    runCode (Repeat n s) = concat $ replicate n s

solve2 :: [Code] -> Int
solve2 = sum . map runCode
  where
    runCode (Raw s) = length s
    runCode (Repeat n s) = n * (solve2 $ parse s)
