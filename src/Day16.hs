module Day16 where

import Data.Bits
import Data.ByteString as BS
import qualified Data.List as List
import Data.List.Split
import Data.Monoid
import Data.Word

type BS = BS.ByteString
type BB = [Bool]

input = "11101000110010100"

len1 :: Int
len1 = 272

len2 :: Int
len2 = 35651584

toBS :: String -> BS
toBS = pack . List.map (\c -> if c == '1' then 1 else 0)

bs2bb :: BS -> BB
bs2bb = List.map (== 1) . BS.unpack

bb2str :: BB -> String
bb2str = List.map (\c -> if c then '1' else '0')

fromBS :: BS -> String
fromBS = List.map (\c -> if c == 1 then '1' else '0') . unpack

dragon :: BS -> BS
dragon s = s <> singleton 0 <> BS.reverse (BS.map (1 -) s)

produce :: BS -> Int -> BS
produce s len = BS.take len $ List.last $ List.unfoldr foo s
  where
    foo s = if BS.length s >= len then Nothing else Just (s', s')
      where
        s' = dragon s

merge :: BS -> BS
merge s = go s empty
  where
    go s acc = if BS.null s then acc
               else go (BS.drop 2 s) (snoc acc z)
      where
        x = BS.head s
        y = BS.head $ BS.tail s
        z = if x == y then 1 else 0

checksum :: BB -> BB
checksum s
  | odd (List.length s) = s
  | otherwise = checksum $ List.map same $ chunksOf 2 s
    where
      same [a, b] = a == b

solve = bb2str . checksum . bs2bb. produce (toBS input)
