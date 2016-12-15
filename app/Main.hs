module Main where

import Day1 as D1
import Day4 as D4
import Day5 as D5
import Day14 as D14

main1 :: IO ()
main1 = do
    l <- getLine
    print $ D1.solve1 l

main4 = D4.solve2 . D4.load <$> D4.input

main5 =
    mapM print $ hashes "reyedfim" [10000000..]

main14 =
    mapM putStrLn $ map (stretch . D14.mkHash input1) $ [0..50000]

main = main14
