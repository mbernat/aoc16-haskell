module Main where

import Day1 as D1
import Day4 as D4
import Day5 as D5
import Day11 as D11
import Day14 as D14
import Day16 as D16

main1 :: IO ()
main1 = do
    l <- getLine
    print $ D1.solve1 l

main4 = D4.solve2 . D4.load <$> D4.input

main5 =
    mapM print $ D5.hashes "reyedfim" [10000000..]

main11 =
    mapM putStrLn
        . map (\(n, s) -> show (n, D11.steps s, D11.distance s))
        . map (\(n, t) -> (n, D11.active t))
        $ (zip [0..] D11.solve1)

main14 =
    mapM putStrLn $ map (stretch . D14.mkHash input1) $ [0..50000]

main16 = print $ D16.solve D16.len2

main = main16
