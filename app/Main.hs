module Main where

import Day1 as D1
import Day2 as D2

main1 :: IO ()
main1 = do
    l <- getLine
    print $ D1.solve1 l

main = main1
